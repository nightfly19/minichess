(ns elo100.core
  (:require [minichess.human :as human]
            [minichess.play :as play])
  (:use elo100.negamax
        elo100.heuristic))

(defn iterative-deepening [search depth time-limit]
  (let [result (ref :nothing)
        deep-searcher (fn []
                        (doseq [depth (range 0 depth)]
                          (dosync
                           (ref-set result (search depth)))))
        thread (Thread. deep-searcher)]
    (.start thread)
    (Thread/sleep (* 1000 time-limit))
    (.stop thread)
    @result))

(defn bot-move [state]
  (iterative-deepening (partial negamax state score true) 10 5))

(defn play-bot []
  (play/play bot-move human/human-move))

(defn -main []
  (println "Playing a game of minichess against elo100")
  (play-bot))

