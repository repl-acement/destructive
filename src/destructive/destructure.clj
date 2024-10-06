(ns destructive.destructure
  (:require
    [clojure.spec.alpha :as s]
    [clojure.core.specs.alpha :as specs]
    [destructive.form-reader :as form-reader]))

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

;; A less general version of get that works only
;; on maps with keywords as keys
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
        :lookup ::map-lookup))

(defn lookup-symbols
  "Read the data and resolve any symbols.
  Return the updated map of symbols."
  [parsed-data symbol-name]
  (let [k (keyword (name symbol-name))
        v {:name (name symbol-name)}
        ref (get parsed-data k)]
    (assoc parsed-data
      symbol-name {k (cond-> v
                             ref (assoc :ref ref))})))

(defn parse-init-expr
  [parsed-data k expr-form]
  (let [conform-result (s/conform ::form expr-form)]
    (cond
      ;; TODO
      ;; need a better "get out of jail" card
      (s/invalid? conform-result)
      parsed-data

      (map? expr-form)
      (assoc parsed-data k {:literal expr-form})

      (contains? #{:get :lookup} (first conform-result))
      (let [[form-name {:keys [key map] :as conformed-form}] conform-result]
        (assoc parsed-data
          k {:form form-name
             :conformed-form conformed-form
             :key key
             :map (let [[map-type v] map]
                    (condp = map-type
                      :symbol {:ref v}
                      {map-type v}))}))

      :else
      parsed-data)))

(defn add-binding-symbols
  [[form-name {:keys [bindings] :as parsed-form}]]
  (assoc {}
    :form-name form-name
    :parsed-form parsed-form
    :bindings bindings
    :bindings-symbols (reduce (fn [symbols {:keys [form init-expr]}]
                                (let [sym (last form)]
                                  (-> symbols
                                      (lookup-symbols sym)
                                      (parse-init-expr sym init-expr))))
                              {}
                              bindings)
    :exprs (:exprs parsed-form)
    :exprs-symbols [:TODO]))

(defn parse
  [form]
  (let [conformant-form (s/conform ::form form)]
    (if (= ::s/invalid conformant-form)
      {:error :unsupported-form
       :form form}
      (add-binding-symbols conformant-form))))

(defn map-accessor?
  [syms {:keys [form] :as sym-data}]
  (when (contains? #{:get :lookup} form)
    (let [{:keys [map]} sym-data]
      (some? (get syms (:ref map))))))

(defn- bindings-symbols->key-access-map
  [bindings-symbols]
  (->> bindings-symbols
       (keep (fn [[k {:keys [map] :as v}]]
               (when (map-accessor? bindings-symbols v)
                 [k (:ref map)])))
       (group-by last)
       (mapv (fn [[m ks]]
               [m (mapv first ks)]))))

(defn drop-accessors
  "Per key set in the access map, remove the redundant
  accessor bindings eg drop the binding for [:local-symbol x]
  when x is in the key set of the access map"
  [access-map bindings]
  (->> access-map
       (mapv (fn [[_ ks]]
               (remove (fn [{:keys [form]}]
                         (contains? (set ks) (last form)))
                       bindings)))
       flatten
       vec))

(defn add-destructurings
  [access-map bindings]
  "Per key set in the access map, add the destructuring bindings"
  (->> access-map
       (mapv (fn [[m ks]]
               {:form [:map-destructure {:keys ks}], :init-expr m}))
       (concat bindings)
       vec))

(defn update-bindings
  [bindings access-map]
  (->> bindings
       (drop-accessors access-map)
       (add-destructurings access-map)))

(defn- let-form->destructured-let
  [form]
  ;; TODO ... have a clearer data structure
  #_{:parse {}
     :analysis {}
     :transform {}
     :output {}}

  (let [{:keys [bindings bindings-symbols parsed-form]} (parse form)
        access-map (bindings-symbols->key-access-map bindings-symbols)
        updated-bindings (update-bindings bindings access-map)
        unform-data (assoc parsed-form :bindings updated-bindings)]
    {:unformed (s/unform ::let unform-data)
     :unform-data unform-data}))

(defn let->destructured-let
  [form-str]
  (let [forms (form-reader/message->forms form-str)]
    (if-not (= 1 (count forms))
      {:error :only-one-form-supported
       :forms forms}
      (let-form->destructured-let (first forms)))))

(comment

  (let [in-exprs '(let [m {:a/k1 1 :k2 2 :k3 3}
                        k1 (:a/k1 m)]
                    (+ k1 k1))]
    ; => transform to
    #_{:name let,
       :bindings [{:form [:local-symbol m], :init-expr {:a/k1 1, :a/k2 2, :k3 3}}
                  {:form [:map-destructure #:a{:keys [k1 k2]}], :init-expr m}],
       :exprs (+ k1 k2)}
    ; => unform to
    #_(let [m {:a/k1 1, :a/k2 2, :k3 3}
            #:a{:keys [k1 k2]} m]
        (+ k1 k2))
    (clojure.pprint/pprint
      (let->destructured-let (pr-str in-exprs))))


  ;; `get` in expression list
  (let [in-exprs '(let [m {:k1 1 :k2 2 :k3 3}]
                    (get m :k1))]
    ; => transform to
    #_(let [m {:k1 1 :k2 2 :k3 3}
            {:keys [k1]} m]
        k1)
    (clojure.pprint/pprint
      (let->destructured-let (pr-str in-exprs))))

  )

