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

(defn negamax
  ([state heuristic depth]
     (swap! cow-count (fn [_] 0))
     (let [temp (negamax state heuristic depth vanilla-pgs)]
       ;;(println @cow-count)
       (:move temp)))
  ([state heuristic depth best]
     (swap! cow-count inc)
     ;;(when (= 0 (mod @cow-count 100))
     ;;  (do (println @cow-count) (flush)))
     (if (or (not= :ongoing (game-status state))
             (<= depth 0))
       (assoc best :alpha (heuristic state))
       (reduce (fn [best-move possible-move]
                 (if (prune? best-move)
                   best-move
                   (update-pgs best-move
                               (assoc (negater (negamax (apply-move state possible-move) heuristic (dec depth) (invert-ab best-move)))
                                 :move possible-move))))
               best
               (possible-moves state)))))
