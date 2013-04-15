(ns elo100.core-test
  (:use clojure.test
        elo100.core
        minichess.core))

(deftest thing-test
  (testing "something"
    (is (= true true))))

(def board {:on-move :white
            :turn 20
            :board [(vec ".kbnr")
                    (vec "qpppp")
                    (vec "p....")
                    (vec "N.PP.")
                    (vec "PPBQP")
                    (vec "R...K")]})

(deftest ab-prune-test
  (testing "Does ab pruned results match normal negamax in inital-game-state"
    (let [unpruned (depth-limited-bot-move initial-game-state 3 false)
          pruned (depth-limited-bot-move initial-game-state 3 true)]
      (is (= (:move unpruned) (:move pruned)))))
  (testing "Does ab pruned results match normal negamax in $random-game-state"
    (let [unpruned (depth-limited-bot-move board 3 false)
          pruned (depth-limited-bot-move board 3 true)]
      (is (= (:move unpruned) (:move pruned)))))
  (testing "Does ab pruned results match normal negamax in $random-game-state"
    (let [unpruned (depth-limited-bot-move board 3 false)
          pruned (depth-limited-bot-move board 3 true)]
      (is (= (:move unpruned) (:move pruned))))))

