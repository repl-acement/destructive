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
      (is (= 1 (eval result)))))
  (testing "Parses a key using lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k2 (:k2 m)]
                         k2)
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 2 (eval result)))))
  (testing "Parses keys using get and lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)]
                         (+ k1 k2))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval result)))))
  (testing "Parser does not optimize access to literal maps"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)
                             k4 (:k4 {:k4 3 :kx 0})]
                         (* k1 k2 k4))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 6 (eval result))))))