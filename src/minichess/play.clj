(ns minichess.play
  (:use minichess.core
        minichess.external)
  (:gen-class))

(defn play
  ([white-player black-player]
     (print-state initial-game-state)
     (println)
     (play initial-game-state white-player black-player))
  ([state white-player black-player]
     (let [status (game-status state)]
       (if (= status :ongoing)
         (let [player-move ((if (= (:on-move state) :white) white-player black-player) state)]
           (print-move player-move)
           (println)
           (let [new-state (-> state
                               (#(apply-move %1 player-move)))]
             (print-state new-state)
             (println)
             (play new-state white-player black-player)))
         (do
           (println (str status))
           {:outcome status
            :state state})))))


