(ns minichess.human
  (:use minichess.core
        minichess.external))

(defn prompt-human-move [state]
     (println "Move: ")
     (try
       (let [move (read-move)]
         (if (move-legal? state move)
           move
           (do
             (println "Illegal move")
             (prompt-human-move state))))
       (catch Exception e
         (println "error reading move")
         (prompt-human-move state))))

(defn human-move [state]
  {:move (prompt-human-move state)})

