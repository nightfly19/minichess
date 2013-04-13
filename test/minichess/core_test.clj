(ns minichess.core-test
  (:use clojure.test
        minichess.core))

(defmacro defpiecetest
  ([name piece coord possible-destinations]
     `(defpiecetest ~name ~piece ~coord ~(update-location empty-board coord piece) ~possible-destinations))
  ([name piece coord board possible-destinations]
     `(deftest ~name
        (let [board# ~board
              moves# (movelist board# ~coord)]
          (testing "movelist returns a set"
            (is (set? moves#)))
          (testing "all moves in set are vectors"
            (is (reduce #(and %1 (move? %2)) true moves#)))
          (testing "all moves in set are in bounds"
            (is (reduce #(and %1 (in-bounds (to %2))) true moves#)))
          (testing "all possible moves are listed"
            (is (= ~possible-destinations (reduce #(conj %1 (to %2)) #{} moves#))))))))

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

(defpiecetest night-test-simple \N [2 2]
  #{[1 0] [3 0]
    [0 1] [4 1]
    [0 3] [4 3]
    [1 4] [3 4]})

(defpiecetest queen-test-simple \Q [2 2]
  #{[0 0] [2 0] [4 0]
    [1 1] [2 1] [3 1]
    [0 2] [1 2] [3 2] [4 2]
    [1 3] [2 3] [3 3]
    [0 4] [2 4] [4 4]
    [2 5]})

(defpiecetest king-test-simple \K [2 2]
  #{[1 1] [2 1] [3 1]
    [1 2] [3 2]
    [3 3] [2 3] [1 3]})

(defpiecetest rook-test-simple \R [2 2]
  #{[2 0]
    [2 1]
    [0 2] [1 2] [3 2] [4 2]
    [2 3]
    [2 4]
    [2 5]})

(defpiecetest bishop-test-simple \B [2 2]
  #{[0 0] [4 0]
    [1 1] [2 1] [3 1]
    [1 2] [3 2]
    [1 3] [2 3] [3 3]
    [0 4] [4 4]})

(defpiecetest pawn-test-white-simple \P [2 2] #{[2 1]})

(defpiecetest pawn-test-black-simple \p [2 2] #{[2 3]})

(defpiecetest pawn-test-white-capture \P [2 2] (update-location initial-board [2 2] \P)
  #{[1 1] [3 1]})
