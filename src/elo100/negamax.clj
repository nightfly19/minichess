(ns elo100.negamax
  (:use minichess.core))

(def win-threshold 10000)
(def lose-threshold -10000)

(def vanilla-pgs
  "Default values for a pgs"
  {:alpha (dec lose-threshold)
   :beta (inc win-threshold)})

(defn negater [pgs]
  "Inverts the alpha value of the pgs"
  (assoc pgs :alpha (* -1 (:alpha pgs))))

(defn prune? [pgs]
  "Prune when the alpha exceeds beta"
  (>= (:alpha pgs) (:beta pgs)))
;;  false)

(defn update-pgs [best current]
  (if (> (:alpha current) (:alpha best))
    (do
      (-> best
          (assoc :alpha (:alpha current))
          (assoc :move (:move current))))
    best))

(defn invert-ab [pgs]
  (let [alpha (:alpha pgs)]
    (-> pgs
        (assoc :alpha (* -1 (:beta pgs)))
        (assoc :beta (* -1 alpha)))))

(def cow-count (atom 0))

(defn random-ordered-possible-moves [state]
  (sort-by (fn [_] (rand-int 10))
           (possible-moves state)))

(defn ordered-possible-moves [state heuristic]
  (sort-by (fn [move] (heuristic (apply-move state move)))
           (possible-moves state)))

(defn negamax
  ([state heuristic prune depth]
     (let [counter (atom 0)
           start-time (System/nanoTime)]
       (-> (negamax state heuristic prune depth counter vanilla-pgs)
           (assoc :nodes @counter)
           (assoc :time (/ (double (- (System/nanoTime) start-time)) 1000000))
           (assoc :depth depth))))

  ([state heuristic prune depth counter best]
     (swap! counter inc)
     (if (or (not= :ongoing (game-status state))
             (<= depth 0))
       (assoc best :alpha (heuristic state))
       (reduce (fn [best-move possible-move]
                 (if (and prune (prune? best-move))
                   best-move
                   (update-pgs best-move
                               (assoc (negater (negamax (apply-move state possible-move) heuristic prune (dec depth) counter
                                                        (invert-ab best-move)))
                                 :move possible-move))))
               best
               (ordered-possible-moves state heuristic)))))
