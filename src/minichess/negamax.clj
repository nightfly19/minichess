(ns minichess.negamax
  (:use minichess.core))

(def win-threshold 10000)
(def lose-threshold -10000)

(defn score-ordered-possibilities [state move-gen heuristic]
  (sorted-set-by #(> (:score %1) (:score %2))
                 (map (fn [result] {:move (first result)
                                    :score (heuristic (nth result 1))})
                      (move-gen state))))

(defn select-best [move-a move-b]
  (if (> (:score move-a) (:score move-b))
    move-a
    move-b))

(defn negater [score depth]
  (if (= 0 (mod depth 2))
    score
    (- score)))

(defn negamax
  ([state heuristic depth]
     (if (or (not= :ongoing (game-status state))
             (<= depth 0))
       {:score (negater (heuristic state) depth)
        :move nil}
       (reduce (fn [best-move possible-move]
                 (select-best best-move
                              (assoc (negamax (apply-move state possible-move) heuristic (dec depth))
                                :move possible-move)))
               {:score (dec lose-threshold)
                :move nil}
               (possible-moves state)))))
