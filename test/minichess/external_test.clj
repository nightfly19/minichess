(ns minichess.external-test
  (:use clojure.test
        minichess.core
        minichess.external))

(deftest deserialize-coord-test
  (testing "Is sane"
    (is (= [[0 0] [4 5]] (deserialize-move "a1-e6")))
    (is (= [[2 3] [1 2]] (deserialize-move "c4-b3")))))
