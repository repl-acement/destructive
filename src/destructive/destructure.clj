(ns destructive.destructure
  (:require
    [clojure.core.specs.alpha :as specs]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.walk :as walk]
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

(s/def ::quoted-sym
  (s/with-gen
    (s/coll-of symbol? :kind list? :distinct true :count 2 :into ())
    #(gen/fmap (fn [sym-name]
                 (list (symbol "quote") (symbol sym-name)))
               (gen/such-that
                 (partial not= "") (gen/string-alphanumeric)))))

(s/def ::key-from-map
  (s/or :keyword keyword?
        :string string?
        :symbol (s/or :sym symbol?
                      :quoted-sym ::quoted-sym)))

;; A spec for `get` that works only on maps, with keywords, strings or symbols as keys
;; It is only applicable for conforming data from :init-expr (the right hand side) of :bindings
(s/def ::get-k-from-m
  (s/cat
    :name (s/and symbol? #(= 'get %))
    :map ::map-value
    :key ::key-from-map
    :default (s/? any?)))

(s/def ::get-in-k-from-m
  (s/cat
    :name (s/and symbol? #(= 'get-in %))
    :map ::map-value
    :keys (s/coll-of ::key-from-map :kind vector? :into [])
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
        :get-in ::get-in-k-from-m
        :lookup ::map-lookup
        :literal-map map?
        :unknown any?))

(defn- lookup-symbol
  "Read the data and attempt to resolve symbol-name.
  Returns the updated map of symbols."
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

        (contains? #{:get :get-in :lookup} form-name)
        (let [{:keys [key keys map]} conformed-form]
          (assoc parsed-data k {:form-name form-name
                                :parsed-form conformed-form
                                :keys keys
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
  "Produce a map for each binding symbol to its fully parsed init-expr."
  [{:keys [bindings]} init-expr-spec]
  (reduce
    (fn [symbols {:keys [form init-expr]}]
      (let [[form-type form-expr] form]
        (if (not= :local-symbol form-type)
          symbols
          (->> symbols
               (lookup-symbol form-expr)
               (parse-init-expr form-expr init-expr-spec init-expr)))))
    {}
    bindings))

(defn- ->parse
  "Add a :parse key to the data map with data from this phase.
  Throws when `form` does not conform to `spec`"
  [{{:keys [edn-form spec]} :inputs :as data}]
  (let [conformant-form (s/conform spec edn-form)]
    (if (= ::s/invalid conformant-form)
      (throw (ex-info "Parse failure" (assoc data :error :unsupported-form)))
      (let [[form-name parsed-form] conformant-form
            bindings-symbols (->binding-symbols parsed-form spec)]
        (assoc data :parse {:form-name form-name
                            :parsed-form parsed-form
                            :bindings-symbols bindings-symbols})))))

;;TODO - code for having X as a `map?` or even just a `symbol?` cos that will
;; need to be good enough where we don't have a literal map in the let bindings.
(defn- map-accessor?
  "Assumes that the map is declared in the bindings"
  [bindings-syms {:keys [form-name] :as sym-data}]
  (and (contains? #{:get :get-in :lookup} form-name)
       (let [{{:keys [ref]} :map} sym-data]
         (some? (get bindings-syms ref)))))

(defn- keyword-meta
  [key-name]
  (cond-> {:type :keyword
           :name (name key-name)}
          (qualified-keyword? key-name)
          (assoc :namespace (namespace key-name))))

(defn- keyword-metadata
  [metadata key-name]
  (merge metadata (keyword-meta key-name)))

(defn- symbol-meta
  [[sym-type sym-name]]
  (cond-> {:type :symbol
           :name (if (= :quoted-sym sym-type)
                   (name (eval sym-name))
                   (name sym-name))}
          (and (= :quoted-sym sym-type)
               (qualified-symbol? (eval sym-name)))
          (assoc :namespace (namespace (eval sym-name)))))

(defn- symbol-metadata
  [metadata key-name]
  (merge metadata (symbol-meta key-name)))

(defn- ->keys-metadata
  [[key-type key-name]]
  (condp = key-type
    :string {:type :string
             :name key-name}
    :keyword (keyword-meta key-name)
    :symbol (symbol-meta key-name)
    (throw (ex-info "Unsupported key type" {:key-type key-type}))))

(defn- keys->map-accessor
  [k {:keys [map keys]}]
  (let [access-path (reduce (fn [result [_ key-name]]
                              (conj result key-name))
                            []
                            keys)
        keys-metadata (mapv (fn [[_key-type key-name :as a-key]]
                              (merge {:value key-name}
                                     (->keys-metadata a-key)))
                            keys)]
    {:symbol (:ref map)
     :access-path access-path
     :accessor k
     :keys keys
     :keys-metadata keys-metadata}))

(defn- key->map-accessor
  [k {:keys [map key]}]
  (let [[key-type key-name] key
        metadata {:value key-name}]
    {:symbol (:ref map)
     :accessor k
     :key key-name
     :key-metadata (condp = key-type
                     :string (merge metadata {:type :string
                                              :name key-name})
                     :keyword (keyword-metadata metadata key-name)
                     :symbol (symbol-metadata metadata key-name)
                     (throw (ex-info "Unsupported key type" {:key-type key-type})))}))

(defn- bindings-symbols->map-accessors
  [bindings-symbols]
  (->> bindings-symbols
       (keep (fn [[k {:keys [key keys] :as v}]]
               (let [accessor? (map-accessor? bindings-symbols v)]
                 (when accessor?
                   (cond
                     keys (keys->map-accessor k v)
                     key (key->map-accessor k v))))))
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

(defn recursive-keys-destructurings
  "Given a map where the keys `highest` is the symbol and the access-path is
  `[:ryan :hi-scores :all-time]`, transform this example binding:
   highest (get-in multiplayer-game-state [:ryan :hi-scores :all-time])
   to this destructuring binding:
   {{{highest :all-time} :hi-scores} :ryan} multiplayer-game-state"
  [access-key {:keys [accessor access-path]}]
  (loop [result {}
         m (reverse access-path)]
    (let [next-kw (first m)]
      (if-not next-kw
        result
        (let [kw-name (name next-kw)]
          ;(prn :recursive-keys-destructurings :result result :kw-name kw-name)
          (recur (cond
                   (and (empty? result)
                        (= (name accessor) kw-name)) (hash-map access-key [accessor])
                   (empty? result) (hash-map accessor next-kw)
                   :else (hash-map result next-kw))
                 (rest m)))))))

(defn k->access-type
  [k]
  (let [t (atom nil)]
    (->> k
         (walk/postwalk
           (fn [x]
             (when (and (map? x)
                        (contains? #{:keys :strs :syms} (first (keys x))))
               (reset! t (first (keys x))))
             x)))
    @t))

(defn merge-kw-keys
  [kw-keys]
  (reduce (fn [result [k v]]
            (cond
              (contains? #{:keys :strs :syms} (first (keys k)))
              (let [t (k->access-type k)
                    b (first (vals k))
                    _ (prn [:k k :v v :t t :b b])]
                (-> result
                    (update :key-bindings (comp vec concat) b)
                    (assoc :access-key t)))

              (map? (first (keys k)))
              result

              :else
              result))
          {:access-key nil
           :key-bindings []}
          kw-keys))

(defn keys-destructurings
  [accessor-data]
  (reduce
    (fn [acc {:keys [accessor key keys key-metadata keys-metadata] :as v}]
      (let [accessor-name (name accessor)]
        (cond
          keys
          ;; need the key type of the last key from the list
          (let [{:keys [namespace type]} (last keys-metadata)
                access-key (condp = type
                             :keyword (keyword namespace "keys")
                             :string :strs
                             :symbol (keyword namespace "syms"))]
            ;(prn :keys-destructurings :acc acc :rkds (recursive-keys-destructurings access-key v))
            (merge acc (recursive-keys-destructurings access-key v)))

          key
          (let [{:keys [namespace name type]} key-metadata
                access-key (condp = type
                             :keyword (keyword namespace "keys")
                             :string :strs
                             :symbol (keyword namespace "syms"))]
            (if (= accessor-name name)
              (update acc access-key #(-> (conj % accessor) vec))
              (merge acc {accessor key}))))))
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
  (let [processed-data (->> data
                            ->parse
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

;; Properly emit qualified keys
(defmethod print-method IPersistentMap
  [m, ^Writer w]
  (#'clojure.core/print-meta m w)
  (#'clojure.core/print-map m #'clojure.core/pr-on w))


(comment

  (let [in-exprs '(let [m {:k {:kk 1}}
                        kk (get m :k)]
                    kk)]
    (let->destructured-let (pr-str in-exprs)))

  (let [in-exprs '(let [multiplayer-game-state {:joe {:class "Ranger"
                                                      :weapon "Longbow"
                                                      :score 100}
                                                :jane {:class "Knight"
                                                       :weapon "Greatsword"
                                                       :score 140}
                                                :ryan {:class "Wizard"
                                                       :weapon "Mystic Staff"
                                                       :hi-scores {:today 200
                                                                   :all-time 290}
                                                       :score 150}}
                        highest (get-in multiplayer-game-state [:ryan :hi-scores :all-time])]
                    highest)]
    (let->destructured-let (pr-str in-exprs)))

  (s/conform ::let '(let [multiplayer-game-state {:joe {:class "Ranger"
                                                        :weapon "Longbow"
                                                        :score 100}
                                                  :jane {:class "Knight"
                                                         :weapon "Greatsword"
                                                         :score 140}
                                                  :ryan {:class "Wizard"
                                                         :weapon "Mystic Staff"
                                                         :hi-scores {:today 200
                                                                     :all-time 290}
                                                         :score 150}}
                          {{{highest :all-time} :hi-scores} :ryan} multiplayer-game-state]
                      highest))

  )
