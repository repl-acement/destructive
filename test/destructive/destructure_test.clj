(ns destructive.destructure-test
  (:require
    [destructive.person :as p]
    [clojure.test :refer [deftest is testing]]
    [destructive.destructure :as sut]))

(deftest unqualified-keys
  (testing "Parses a key using get"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)]
                         k1)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k1]}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parses a key using lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k2 (:k2 m)]
                         k2)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 2 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k2]}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parses keys using get and lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)]
                         (+ k1 k2))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k1 k2]}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parses several keys using get and lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)
                             k3 (:k3 m)]
                         (+ k1 k2 k3))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 6 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k1 k2 k3]}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parser does not optimize access to literal maps"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)
                             k4 (:k4 {:k4 3 :kx 0})]
                         (* k1 k2 k4))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 6 (eval (-> result :unform :unformed))))
      (is (= 3 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k1 k2]}], :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parser does not affect other bindings"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             x 38
                             y [1 2]
                             z (vec (range 3))]
                         (apply + k1 x z))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 42 (eval (-> result :unform :unformed))))
      (is (= 5 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:keys [k1]}], :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last))))))

(deftest qualified-keys
  (testing "Parses a key using get"
    (let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                             k1 (get m :a/k1)]
                         k1)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:a/keys [k1]}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parses a key using lookup"
    (let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                             k2 (:b/k2 m)]
                         k2)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 2 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:b/keys [k2]}]
               :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parses keys using get and lookup"
    (let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                             k1 (get m :a/k1)
                             k2 (:b/k2 m)]
                         (+ k1 k2))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:a/keys [k1] :b/keys [k2]}]
               :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parser does not optimize access to literal maps"
    (let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                             k1 (get m :a/k1)
                             k2 (:b/k2 m)
                             k4 (:k4 {:k4 3 :kx 0})]
                         (* k1 k2 k4))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 6 (eval (-> result :unform :unformed))))
      (is (= 3 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:a/keys [k1] :b/keys [k2]}]
               :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last)))))
  (testing "Parser does not affect other bindings"
    (let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                             k1 (get m :a/k1)
                             x 38
                             y [1 2]
                             z (vec (range 3))]
                         (apply + k1 x z))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 42 (eval (-> result :unform :unformed))))
      (is (= 5 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:a/keys [k1]}]
               :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last))))))

(deftest binding-not-keyname
  (testing "When a key name is not the same as the binding"
    (let [in-bindings '(let [m {:an-ns/k1 1 :k2 2}
                             k1 (:an-ns/k1 m)
                             x (:k2 m)]
                         (+ k1 x))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:an-ns/keys [k1]
                                        x :k2}]
               :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last)))))
  (testing "When a qualified key name is not the same as the binding"
    (let [in-bindings '(let [m {:an-ns/k1 1 :an-ns/k2 2}
                             k1 (:an-ns/k1 m)
                             x (:an-ns/k2 m)]
                         (+ k1 x))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:an-ns/keys [k1]
                                        x :an-ns/k2}]
               :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last))))))

(deftest nested-simple-keys
  (testing "An unqualified key is accessed via get-in"
    (let [in-bindings '(let [m {:k {:kk 1}}
                             kk (get-in m [:k :kk])]
                         kk)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:keys [kk]} :k}], :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "An unqualified key is accessed via get-in can be renamed"
    (let [in-bindings '(let [m {:k {:kk 1}}
                             xx (get-in m [:k :kk])]
                         xx)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{xx :kk} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Several unqualified keys are possible"
    (let [in-bindings '(let [m {:k {:kk 1} :x {:xx 2}}
                             kk (get-in m [:k :kk])
                             xx (get-in m [:x :xx])]
                         (+ kk xx))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:keys [kk]} :k
                                        {:keys [xx]} :x}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Several unqualified keys that access the same key are merged"
    (let [in-bindings '(let [m {:k {:kk 1 :mm 2}}
                             kk (get-in m [:k :kk])
                             mm (get-in m [:k :mm])]
                         (+ kk mm))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:keys [mm kk]} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last))))))

(deftest nested-qualified-keys
  (testing "An qualified key is accessed via get-in"
    (let [in-bindings '(let [m {:k {:a/kk 1}}
                             kk (get-in m [:k :a/kk])]
                         kk)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:a/keys [kk]} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "An qualified key is accessed via get-in can be renamed"
    (let [in-bindings '(let [m {:k {:a/kk 1}}
                             xx (get-in m [:k :a/kk])]
                         xx)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 1 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{xx :a/kk} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Several qualified keys are possible"
    (let [in-bindings '(let [m {:k {:a/kk 1} :x {:b/xx 2}}
                             kk (get-in m [:k :a/kk])
                             xx (get-in m [:x :b/xx])]
                         (+ kk xx))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:a/keys [kk]} :k
                                        {:b/keys [xx]} :x}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Several qualified keys that access the same key are merged"
    (let [in-bindings '(let [m {:k {:a/kk 1 :a/mm 2}}
                             kk (get-in m [:k :a/kk])
                             mm (get-in m [:k :a/mm])]
                         (+ kk mm))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:a/keys [kk mm]} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Several qualified keys from different namespaces are not merged"
    (let [in-bindings '(let [m {:k {:a/kk 1 :b/mm 2}}
                             kk (get-in m [:k :a/kk])
                             mm (get-in m [:k :b/mm])]
                         (+ kk mm))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:a/keys [kk]
                                         :b/keys [mm]} :k}] :init-expr m}
             (-> result :unform :unform-form :bindings last))))))

(deftest autoresolved-keywords
  (testing "An auto-resolved key is accessed via get"
    (let [in-bindings '(let [m {::p/role-name "Smashy"}
                             role-name (get m ::p/role-name)]
                         role-name)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Smashy" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      ;; not quite right, is
      (is (= '{:form [:map-destructure {:destructive.person/keys [role-name]}]
               :init-expr m}
             (-> result :unform :unform-form :bindings last)))
      ;; should be ... though both pass the tests :)
      (is (= '{:form [:map-destructure {::p/keys [role-name]}]
               :init-expr m}
             (-> result :unform :unform-form :bindings last)))))
  )

(deftest destructuring-guide-examples
  (testing "simple key bindings are destructured into a map"
    (let [in-bindings '(let [client {:name "Super Co."
                                     :location "Philadelphia"
                                     :description "The worldwide leader in plastic tableware."}
                             the-name (:name client)
                             the-location (:location client)
                             the-description (:description client)]
                         (str the-name " " the-location " - " the-description))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Super Co. Philadelphia - The worldwide leader in plastic tableware."
             (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {the-description :description
                                        the-location :location
                                        the-name :name}]
               :init-expr client}
             (-> result :unform :unform-form :bindings last)))))
  (testing "Incorrect bindings are nil"
    (let [in-bindings '(let [client {:name "Super Co."
                                     :location "Philadelphia"
                                     :description "The worldwide leader in plastic tableware."}
                             {category :category} client]
                         category)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (nil? (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {category :category}]
               :init-expr client}
             (-> result :unform :unform-form :bindings last)))))

  (testing "Default bindings are supported"
    (let [in-bindings '(let [client {:name "Super Co."
                                     :location "Philadelphia"
                                     :description "The worldwide leader in plastic tableware."}
                             {category :category
                              :or {category "Category not found"}} client]
                         category)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Category not found" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:or {category "Category not found"}
                                        category :category}]
               :init-expr client}
             (-> result :unform :unform-form :bindings last)))))

  (testing "Whole map :as X is supported"
    (let [in-bindings '(let [client {:name "Super Co."
                                     :location "Philadelphia"
                                     :description "The worldwide leader in plastic tableware."}
                             {name :name :as all} client]
                         (= all client))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (true? (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {name :name :as all}]
               :init-expr client}
             (-> result :unform :unform-form :bindings last)))))

  (testing "Whole map :as X and :or are supported"
    (let [in-bindings '(let [my-map {:a "A" :b "B" :c 3 :d 4}
                             {x :x :or {x "Not found"} :as all} my-map]
                         (and (= x "Not found")
                              (= all my-map)))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (true? (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {x :x :or {x "Not found"} :as all}]
               :init-expr my-map}
             (-> result :unform :unform-form :bindings last)))))

  (testing ":strs is used for access to string keys (JSON anyone?)"
    (let [in-bindings '(let [string-keys {"first-name" "Joe"
                                          "last-name" "Smith"}
                             first-name (get string-keys "first-name")]
                         first-name)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Joe" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:strs [first-name]}]
               :init-expr string-keys}
             (-> result :unform :unform-form :bindings last)))))

  (testing ":syms is used for access to symbol keys (parsers anyone?)"
    (let [in-bindings '(let [symbol-keys {'first-name "Jane"
                                          'last-name "Doe"}
                             first-name (get symbol-keys 'first-name)]
                         first-name)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Jane" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:syms [first-name]}]
               :init-expr symbol-keys}
             (-> result :unform :unform-form :bindings last)))))

  (testing ":syms can be used to access namespaced symbols"
    (let [in-bindings '(let [symbol-keys {'an-ns/first-name "Jane"
                                          'an-ns/last-name "Doe"}
                             first-name (get symbol-keys 'an-ns/first-name)]
                         first-name)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Jane" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {:an-ns/syms [first-name]}]
               :init-expr symbol-keys}
             (-> result :unform :unform-form :bindings last)))))

  (testing "associative destructuring can be nested"
    (let [in-bindings '(let [multiplayer-game-state {:joe {:class "Ranger"
                                                           :weapon "Longbow"
                                                           :score 100}
                                                     :jane {:class "Knight"
                                                            :weapon "Greatsword"
                                                            :score 140}
                                                     :ryan {:class "Wizard"
                                                            :weapon "Mystic Staff"
                                                            :score 150}}
                             class (get-in multiplayer-game-state [:joe :class])]
                         class)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Ranger" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:keys [class]} :joe}]
               :init-expr multiplayer-game-state}
             (-> result :unform :unform-form :bindings last)))))

  (testing "associative destructuring can be nested with renames"
    (let [in-bindings '(let [multiplayer-game-state {:joe {:class "Ranger"
                                                           :weapon "Longbow"
                                                           :score 100}
                                                     :jane {:class "Knight"
                                                            :weapon "Greatsword"
                                                            :score 140}
                                                     :ryan {:class "Wizard"
                                                            :weapon "Mystic Staff"
                                                            :score 150}}
                             joe-class (get-in multiplayer-game-state [:joe :class])]
                         joe-class)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= "Ranger" (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{joe-class :class} :joe}]
               :init-expr multiplayer-game-state}
             (-> result :unform :unform-form :bindings last)))))

  (testing "and can be combined with sequential destructuring as needed"
    (let [in-bindings '(let [multiplayer-game-state {:joe {:class "Ranger"
                                                           :weapon "Longbow"
                                                           :score 100}
                                                     :jane {:class "Knight"
                                                            :weapon "Greatsword"
                                                            :score 140}
                                                     :ryan {:class "Wizard"
                                                            :weapon "Mystic Staff"
                                                            :score 150}}
                             class (get-in multiplayer-game-state [:joe :class])
                             weapon (get-in multiplayer-game-state [:joe :weapon])]
                         [class weapon])
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= ["Ranger" "Longbow"] (eval (-> result :unform :unformed))))
      (is (= 2 (count (-> result :unform :unform-form :bindings))))
      (is (= '{:form [:map-destructure {{:keys [weapon class]} :joe}]
               :init-expr multiplayer-game-state}
             (-> result :unform :unform-form :bindings last))))))





