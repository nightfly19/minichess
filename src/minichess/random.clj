(ns minichess.random
  (:use minichess.core
        minichess.external
        minichess.play))

(defn random-player [state]
  (let [moves (possible-moves state)]
    (nth (seq moves) (rand-int (dec (count moves))))))

(defn random-game []
  (minichess.play/play random-player random-player))

