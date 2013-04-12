(ns minichess.human
  (:use minichess.core
        minichess.external))

(defn human-move [state]
  (println "Move: ")
  (try
    (let [move (read-move)]
      (if (move-legal? state move)
        move
        (do
          (println "Illegal move")
          (human-move state))))
    (catch Exception e
      (println "error reading move")
      (human-move state))))

