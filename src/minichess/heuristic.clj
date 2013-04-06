(ns minichess.heuristic
  (:use minichess.core))

(def piece-values
  {\P 100
   \B 300
   \N 300
   \R 500
   \Q 900
   \K 10000})

(defn piece-points [board color]
  (reduce +
          (map #(get piece-values (-> board (piece-at %1) (piece-class)))
               (locations-of-color board color))))

(defn score [state]
  (let [piece-score (piece-points (:board state) (:on-move state))
        opp-piece-score (piece-points (:board state) (opp-color (:on-move state)))
        status (game-status state)]
    (cond
     (= :draw status) 0
     (= :ongoing status) (- piece-score opp-piece-score)
     :default (if (= (:on-move state) status)
                10000
                -10000))))
