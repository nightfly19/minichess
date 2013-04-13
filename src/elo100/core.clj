(ns elo100.core
  (:require [minichess.human :as human]
            [minichess.play :as play])
  (:use elo100.negamax
        elo100.heuristic))

(defn bot-move [state]
  (negamax state score 4))

(defn play-bot []
  (play/play bot-move human/human-move))

(defn -main []
  (println "Playing a game of minichess against elo100")
  (play-bot))
             
