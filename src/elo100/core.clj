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

(defn time-limited-bot-move
  ([state time-limit]
     (time-limited-bot-move state time-limit true))
  ([state time-limit pruning]
     (iterative-deepening (partial negamax state score pruning) 10 time-limit)))

(defn depth-limited-bot-move
  ([state max-depth]
     (depth-limited-bot-move state max-depth true))
  ([state max-depth pruning]
     (negamax state score pruning max-depth)))

(defn play-bot-time-limited [time-limit]
  (play/play #(time-limited-bot-move %1 time-limit) human/human-move))

(defn play-bot-depth-limited [depth-limit]
  (play/play #(depth-limited-bot-move %1 depth-limit) human/human-move))

(defn bot-vs-bot-time-limited [time-limit]
  (play/play #(time-limited-bot-move %1 time-limit)
             #(time-limited-bot-move %1 time-limit)))

(defn -main []
  (println "Playing a game of minichess against elo100")
  (bot-vs-bot-time-limited 5))

