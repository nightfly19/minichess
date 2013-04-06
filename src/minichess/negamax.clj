(ns minichess.negamax
  (:use minichess.core))

(def win-threshold 10000)
(def lose-threshold -10000)

(defn score-ordered-possibilities [state move-gen heuristic]
  (sorted-set-by #(> (:score %1) (:score %2))
                 (map (fn [result] {:move (first result)
                                    :score (heuristic (nth result 1))})
                      (move-gen state))))

;;(defn negate [state test-fn]

(defn negamax
  ([state heuristic depth]
     (if (or (not= :ongoing (game-status state))
             (<= depth 0))
       (heuristic state)
       (possible-moves state))))
