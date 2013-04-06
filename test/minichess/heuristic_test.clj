(ns minichess.heuristic-test
  (:use clojure.test
        minichess.core
        minichess.heuristic))

(deftest score-test
  (testing "initial even position"
    (is (= 0 (score initial-game-state))))
  (testing "white extra pawn"
    (is (= 100 (score (assoc initial-game-state
                        :board (update-location initial-board [2 2] \P))))))
  (testing "black without king"
    (is (= 10000 (score (assoc initial-game-state
                          :board (update-location initial-board [0 0] \.))))))
  (testing "white without king"
    (is (= -10000 (score (assoc initial-game-state
                          :board (update-location initial-board [4 5] \.)))))))
