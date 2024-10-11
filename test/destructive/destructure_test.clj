(ns destructive.destructure-test
  (:require
    [clojure.test :refer [is deftest testing]]
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
      (is (= '{:form [:map-destructure {:b/keys [k2]}], :init-expr m}
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
      (is (= '{:form [:map-destructure {:a/keys [k1] :b/keys [k2]}], :init-expr m}
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
      (is (= '{:form [:map-destructure {:a/keys [k1] :b/keys [k2]}], :init-expr m}
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
      (is (= '{:form [:map-destructure {:a/keys [k1]}], :init-expr m}
             ;; TODO ... does placement of destructuring in the bindings list matter?
             (-> result :unform :unform-form :bindings last))))))