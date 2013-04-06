(ns minichess.core-test
  (:use clojure.test
        minichess.core))

(deftest in-bounds-test
  (testing "Is sane"
    (is (= true (in-bounds [0 0])))
    (is (= true (in-bounds [x-upper y-upper])))
    (is (= true (in-bounds [(dec x-upper) (dec y-upper)])))
    (is (= false (in-bounds [(inc x-upper) y-upper])))
    (is (= false (in-bounds [x-upper (inc y-upper)])))
    (is (= false (in-bounds [(inc x-upper) (inc y-upper)])))
    (is (= false (in-bounds [(inc x-upper) (inc y-upper)])))))

(deftest color-at-test
  (testing "Is sane"
    (is (= :black (color-at initial-board [0 0])))
    (is (= :white (color-at initial-board [x-upper y-upper])))
    (is (= nil (color-at initial-board [3 3])))
    (is (= nil (color-at initial-board [(inc x-upper) (inc y-upper)])))))

(deftest valid-landing-test
  (testing "Is sane"
    (is (not= true (valid-landing initial-board :black [0 0])))
    (is (= true (valid-landing initial-board :white [0 0])))
    (is (not= true (valid-landing initial-board :white [x-upper y-upper])))
    (is (= true (valid-landing initial-board :black [x-upper y-upper])))))

(deftest move-scan-test
  (testing "Is sane"
    (is (= (move-scan initial-board [1 1] [1 1])
           #{[[1 1] [4 4]] [[1 1] [3 3]] [[1 1] [2 2]]}))
    (is (= (move-scan initial-board [1 1] [1 1] false)
           #{[[1 1] [3 3]] [[1 1] [2 2]]}))
    (is (= (move-scan initial-board [1 1] [-1 0])
           #{}))
    (is (= (move-scan initial-board [1 1] [0 -1])
           #{}))
    (is (= (move-scan initial-board [0 2] [1 0])
           #{[[0 2] [4 2]] [[0 2] [3 2]] [[0 2] [2 2]] [[0 2] [1 2]]}))
    (is (= (move-scan initial-board [2 2] [1 0])
           #{[[2 2] [4 2]] [[2 2] [3 2]]}))
    (is (= (move-scan initial-board [3 4] [0 -1])
           #{[[3 4] [3 1]] [[3 4] [3 2]] [[3 4] [3 3]]}))))

(deftest moves-for-color-test
  (testing "Super basic test"
    (is (= 7 (count (moves-for-color initial-board :white))))))

(let [state (assoc initial-game-state :on-move :black)]
  (deftest move-legal?-test
    (testing "Is sane"
      (is (= true (move-legal? state [[0 1] [0 2]])))
      (is (= true (move-legal? state [[3 1] [3 2]])))
      (is (= true (move-legal? state [[3 0] [2 2]])))
      (is (= false (move-legal? state [[0 1] [3 2]]))))))
