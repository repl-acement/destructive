(ns destructive.destructure
  (:require
    [clojure.core.specs.alpha :as specs]
    [clojure.spec.alpha :as s]
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

;; A spec for get that works only on maps, with keywords as keys
(s/def ::get-k-from-m
  (s/cat
    :name (s/and symbol? #(= 'get %))
    :map ::map-value
    :key keyword?
    :default (s/? any?)))

(s/def ::map-lookup
  (s/cat
    :key keyword?
    :map ::map-value))

(s/def ::form
  (s/or :let ::let
        :get ::get-k-from-m
        :lookup ::map-lookup
        :literal-map map?
        :unknown any?))

(defn keyword-parts [k]
  (cond-> {:keyword k
           :name (name k)}
          (qualified-keyword? k) (assoc :namespace (namespace k))))

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
  (reduce (fn [symbols {:keys [form init-expr]}]
            (let [sym (last form)]
              (->> symbols
                   (lookup-symbols sym)
                   (parse-init-expr sym init-expr-spec init-expr))))
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

(defn- bindings-symbols->map-accessors
  [bindings-symbols]
  (->> bindings-symbols
       (keep (fn [[k {:keys [map key] :as v}]]
               (when (map-accessor? bindings-symbols v)
                 {:symbol (:ref map)
                  :accessor k
                  :key (keyword-parts key)})))
       (group-by :symbol)))

(defn- ->symbol-accessors
  [{{:keys [bindings-symbols]} :parse :as data}]
  (let [symbol-accessors (bindings-symbols->map-accessors bindings-symbols)]
    (assoc data
      :analysis {:bindings {:map-accessors symbol-accessors}})))

(defn- drop-accessors
  "Per key set in the access map, remove the redundant
  accessor bindings eg drop the binding for [:local-symbol x]
  when x is in the key set of the access map"
  [map-accessors bindings]
  ;; TODO ... this seems ugly, let's refactor!
  (->> map-accessors
       (mapv (fn [[_ accessor-data]]
               (let [accessor-set (->> accessor-data (map :accessor) set)]
                 (remove (fn [{:keys [form]}]
                           (let [local-symbol (last form)]
                             (contains? accessor-set local-symbol)))
                         bindings))))
       flatten
       vec))

#_[{:symbol m, :accessor k1, :key {:keyword :k1, :name "k1"}}
   {:symbol m, :accessor k2, :key {:keyword :k2, :name "k2"}}]

;; target => {:keys [k1 k2]}

#_[{:symbol m,
    :accessor k1,
    :key {:keyword :an-ns/k1, :name "k1", :namespace "an-ns"}}
   {:symbol m,
    :accessor k2,
    :key {:keyword :k2, :name "k2"}}]

;; target => {:an-ns/keys [k1], :keys [k2]}

#_[{:symbol m,
    :accessor x,
    :key {:keyword :an-ns/k2, :name "k2", :namespace "an-ns"}}]

;; target => {x :an-ns/k2}


(defn keys-destructurings
  [accessor-data]
  (reduce
    (fn [acc {:keys [accessor key]}]
      (let [accessor-name (name accessor)
            {:keys [namespace name]} key
            access-key (keyword namespace "keys")]
        (if (= accessor-name name)
          (update acc access-key #(-> (conj % accessor) vec))
          (merge acc {accessor (:keyword key)}))))
    {}
    accessor-data))

(defn- add-destructurings
  [map-accessors bindings]
  "Per key set in the access map, add the destructuring bindings"
  (->> map-accessors
       (mapv (fn [[map-symbol accessor-data]]
               (let [destructurings (keys-destructurings accessor-data)]
                 {:form [:map-destructure destructurings]
                  :init-expr map-symbol})))
       (concat bindings)
       vec))

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

  (let [in-exprs '(let [m {:k1 1 :k2 2 :k3 3}
                        k1 (:k1 m)
                        k2 (:k2 m)
                        k3 (get m :k3)]
                    (+ k1 k2 k3))]
    (let->destructured-let (pr-str in-exprs)))
  ;; ^^^ WORKS


  (let [in-exprs '(let [m {:an-ns/k1 1 :k2 2 :k3 3}
                        k1 (:an-ns/k1 m)
                        k2 (:k2 m)
                        k3 (get m :k3)]
                    (+ k1 k2 k3))]
    (let->destructured-let (pr-str in-exprs)))
  ;; ^^^ WORKS

  (let [in-exprs '(let [m {:an-ns/k1 1 :an-ns/k2 2 :k3 3 :k4 4}
                        k1 (:an-ns/k1 m)
                        x (:an-ns/k2 m)
                        k3 (:k3 m)
                        k4 (get m :k4)]
                    (+ k1 x k3 k4))]
    (let->destructured-let (pr-str in-exprs)))
  ;; ^^^ WORKS


  )

;; Properly emit qualified keys
(defmethod print-method IPersistentMap
  [m, ^Writer w]
  (#'clojure.core/print-meta m w)
  (#'clojure.core/print-map m #'clojure.core/pr-on w))
