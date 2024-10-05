(ns destructive.destructure-test
  (:require
    [clojure.test :refer [is deftest testing]]
    [destructive.destructure :as sut]))

(deftest simple-keys
  (testing "Keys using get and lookup"
    (let [in-bindings '(let [m {:k1 1 :k2 2 :k3 3}
                             k1 (get m :k1)
                             k2 (:k2 m)]
                         (+ k1 k2))
          result (->> (pr-str in-bindings)
                      sut/let->destructured-let)]
      (is (= 3 (eval result))))))