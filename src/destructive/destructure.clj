(ns destructive.destructure
  (:require
    [clojure.core.specs.alpha :as specs]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [destructive.form-reader :as form-reader])
  (:import (clojure.lang IPersistentMap)
           (java.io Writer)))

(set! *warn-on-reflection* true)

(s/def ::map-value
  (s/or :literal map?
        :symbol symbol?))

(s/def ::let
  (s/cat
    :name (s/and symbol? #(= 'let %))
    :bindings (s/and (s/conformer identity vec)
                     ::specs/bindings)
    :exprs (s/? any?)))

(s/def ::let-qualified
  (s/cat
    :name (s/and symbol? #(= 'let %))
    :bindings (s/and (s/conformer identity vec)
                     ::specs/bindings)
    :exprs (s/? any?)))

(s/def ::quoted-sym
  (s/with-gen
    (s/coll-of symbol? :kind list? :distinct true :count 2 :into ())
    #(gen/fmap (fn [sym-name]
                 (list (symbol "quote") (symbol sym-name)))
               (gen/such-that
                 (partial not= "") (gen/string-alphanumeric)))))

;; A spec for `get` that works only on maps, with keywords, strings or symbols as keys
;; It is only applicable for conforming data from :init-expr (the right hand side) of :bindings
(s/def ::get-k-from-m
  (s/cat
    :name (s/and symbol? #(= 'get %))
    :map ::map-value
    :key (s/or :keyword keyword?
               :string string?
               :symbol (s/or :sym symbol?
                             :quoted-sym ::quoted-sym))
    :default (s/? any?)))

(s/def ::map-lookup
  (s/cat
    :key (s/or :keyword keyword?
               :string string?
               :symbol symbol?)
    :map ::map-value))

(s/def ::form
  (s/or :let ::let
        :get ::get-k-from-m
        :lookup ::map-lookup
        :literal-map map?
        :unknown any?))

(defn- lookup-symbols
  "Read the data and resolve any symbols.
  Return the updated map of symbols."
  [symbol-name parsed-data]
  (let [k (keyword (name symbol-name))
        v {:name (name symbol-name)}
        ref (get parsed-data k)
        symbol-data (cond-> v
                            ref (assoc :ref ref))]
    (assoc parsed-data symbol-name {k symbol-data})))

(defn- parse-init-expr
  [k init-expr-spec expr-form parsed-data]
  (let [conform-result (s/conform init-expr-spec expr-form)]
    (if (s/invalid? conform-result)
      (throw (ex-info "Parse init-expr conform failure"
                      {:error :unsupported-form
                       :spec init-expr-spec
                       :form expr-form})))
    (let [[form-name conformed-form] conform-result]
      (cond
        (= :literal-map form-name)
        (assoc parsed-data k {:literal expr-form})

        (contains? #{:get :lookup} form-name)
        (let [{:keys [key map]} conformed-form]
          (assoc parsed-data k {:form-name form-name
                                :parsed-form conformed-form
                                :key key
                                :map (let [[map-type v] map]
                                       (condp = map-type
                                         :symbol {:ref v}
                                         {map-type v}))}))

        (= :unknown form-name)
        parsed-data

        :else
        (throw (ex-info "Parse init-expr unknown failure"
                        {:error :unknown-error
                         :spec init-expr-spec
                         :form expr-form}))))))

(defn- ->binding-symbols
  [{:keys [bindings]} init-expr-spec]
  (reduce
    (fn [symbols {:keys [form init-expr]}]
      (let [[form-type form-expr] form]
        (if (not= :local-symbol form-type)
          symbols
          (->> symbols
               (lookup-symbols form-expr)
               (parse-init-expr form-expr init-expr-spec init-expr)))))
    {}
    bindings))

(defn- parse
  "Add a :parse key to the data map with data from this phase.
  Throws when `form` does not conform to `spec`"
  [{{:keys [edn-form spec]} :inputs :as data}]
  (let [conformant-form (s/conform spec edn-form)]
    (if (= ::s/invalid conformant-form)
      (throw (ex-info "Parse failure" (assoc data :error :unsupported-form)))
      (let [[form-name parsed-form] conformant-form
            ;; TODO should this spec be the same as the incoming spec? Probably!
            bindings-symbols (->binding-symbols parsed-form spec)]
        (assoc data :parse {:form-name form-name
                            :parsed-form parsed-form
                            :bindings-symbols bindings-symbols})))))

(defn- map-accessor?
  [syms {:keys [form-name] :as sym-data}]
  (when (contains? #{:get :lookup} form-name)
    (let [{:keys [map]} sym-data]
      (some? (get syms (:ref map))))))


(defn- keyword-metadata
  [metadata key-value]
  (cond-> (merge metadata {:type :keyword
                           :name (name key-value)})
          (qualified-keyword? key-value)
          (assoc :namespace (namespace key-value))))

(defn- symbol-metadata
  [metadata key-value]
  (let [[sym-type sym-value] key-value]
    (cond-> (merge metadata
                   {:type :symbol
                    :name (if (= :quoted-sym sym-type)
                            (name (eval sym-value))
                            (name sym-value))})
            (and (= :quoted-sym sym-type)
                 (qualified-symbol? (eval sym-value)))
            (assoc :namespace (namespace (eval sym-value))))))

(defn- bindings-symbols->map-accessors
  [bindings-symbols]
  (->> bindings-symbols
       (keep (fn [[k {:keys [map key] :as v}]]
               (when (map-accessor? bindings-symbols v)
                 (let [[key-type key-value] key
                       metadata {:value key-value}]
                   {:symbol (:ref map)
                    :accessor k
                    :key key-value
                    :key-metadata (condp = key-type
                                    :string (merge metadata {:type :string
                                                             :name key-value})
                                    :keyword (keyword-metadata metadata key-value)
                                    :symbol (symbol-metadata metadata key-value))}))))
       (group-by :symbol)))

(defn- ->symbol-accessors
  [{{:keys [bindings-symbols]} :parse :as data}]
  (let [symbol-accessors (bindings-symbols->map-accessors bindings-symbols)]
    (assoc data :analysis {:bindings {:map-accessors symbol-accessors}})))

(defn- drop-accessors
  "Per key set in the access map, remove the redundant
  accessor bindings eg drop the binding for [:local-symbol x]
  when x is in the key set of the access map"
  [map-accessors bindings]
  (if (empty? map-accessors)
    bindings
    (->> map-accessors
         (mapv (fn [[_ accessor-data]]
                 (let [accessor-set (->> accessor-data (map :accessor) set)]
                   (remove (fn [{:keys [form]}]
                             (let [local-symbol (last form)]
                               (contains? accessor-set local-symbol)))
                           bindings))))
         flatten
         vec)))

(defn keys-destructurings
  [accessor-data]
  (reduce
    (fn [acc {:keys [accessor key key-metadata]}]
      (let [accessor-name (name accessor)
            {:keys [namespace name type]} key-metadata
            access-key (condp = type
                         :keyword (keyword namespace "keys")
                         :string :strs
                         :symbol (keyword namespace "syms"))]
        (if (= accessor-name name)
          (update acc access-key #(-> (conj % accessor) vec))
          (merge acc {accessor key}))))
    {}
    accessor-data))

(defn- add-destructurings
  [map-accessors bindings]
  "Per key set in the access map, add the destructuring bindings"
  (let [extra-bindings (->> map-accessors
                            (mapv (fn [[map-symbol accessor-data]]
                                    (let [destructurings (keys-destructurings accessor-data)]
                                      {:form [:map-destructure destructurings]
                                       :init-expr map-symbol}))))]
    (vec (concat bindings extra-bindings))))

(defn- ->destructured-bindings
  [{:keys [parse analysis] :as data}]
  (let [bindings (get-in parse [:parsed-form :bindings])
        map-accessors (get-in analysis [:bindings :map-accessors])
        updated-bindings (->> bindings
                              (drop-accessors map-accessors)
                              (add-destructurings map-accessors))]
    (assoc data :transform {:bindings updated-bindings})))

(defn- ->unform-data
  [{:keys [parse transform] :as data}]
  (let [parsed-form (:parsed-form parse)
        updated-bindings (:bindings transform)
        unform-form (assoc parsed-form :bindings updated-bindings)]
    (assoc data :unform {:unform-form unform-form})))

(defn- let-form->destructured-let
  [data]
  (let [processed-data (->> (parse data)
                            ->symbol-accessors
                            ->destructured-bindings
                            ->unform-data)]
    (->> (get-in processed-data [:unform :unform-form])
         (s/unform ::let)
         (assoc-in processed-data [:unform :unformed]))))

(defn let->destructured-let
  [form-str]
  (let [forms (form-reader/message->forms form-str)
        form-spec ::form]
    (if-not (= 1 (count forms))
      {:error :only-one-form-supported
       :input form-str
       :forms forms}
      (let-form->destructured-let {:inputs {:string-form form-str
                                            :edn-form (first forms)
                                            :spec form-spec}}))))

(comment

  (let [in-exprs '(let [symbol-keys {'a/first-name "Jane"
                                     'a/last-name "Doe"}
                        first-name (get symbol-keys 'a/first-name)]
                    first-name)]
    (let->destructured-let (pr-str in-exprs)))

  )

;; Properly emit qualified keys
(defmethod print-method IPersistentMap
  [m, ^Writer w]
  (#'clojure.core/print-meta m w)
  (#'clojure.core/print-map m #'clojure.core/pr-on w))
