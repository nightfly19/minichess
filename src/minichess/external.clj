(ns minichess.external
  (:require [clojure.string :as string])
  (:use minichess.core))

(set! *warn-on-reflection* true)

(def color-map
  {"W" :white
   "B" :black})

(def color-string
  {:white "W"
   :black "B"})

(defn serialize-coord [coord]
  (str (char (+ (int \a) (x coord))) (y coord)))

(defn serialize-move [move]
  (str (serialize-coord (from move)) "-" (serialize-coord (to move))))

(defn deserialize-coord [coord]
  (let [[_ x y] (first (re-seq #"^(\D)(\d)$" coord))]
    [(- (int (first x)) (int \a))
     (- (. Integer parseInt y) 1)]))

(defn deserialize-move [move-string]
  (let [[_ from to] (first (re-seq #"^(\D\d)-(\D\d)$" move-string))]
    [(deserialize-coord from) (deserialize-coord to)]))

(defn- process-state-line [state line]
  (assoc state :board (into [] (concat (:board state) [(into [] line)]))))

(defn read-move []
  (deserialize-move (string/trim (read-line))))

(defn read-state
  ([]
     (read-state {:board (seq [])} 0))
  ([state line]
     (let [input (string/trimr (read-line))]
       (cond
        (= line 0) (let [[_ whose-turn move] (first (re-seq #"^([WB])\s+(\d+)" input))]
                     (-> state
                         (assoc :on-move (get color-map whose-turn))
                         (assoc :turn (. Integer parseInt move))
                         (read-state (inc line))))
        (< line 6) (read-state (process-state-line state input) (inc line))
        :default (process-state-line state input)))))

(defn print-state [state]
  (println (str (color-string (:on-move state)) " " (:turn state)))
  (loop [rows (:board state)]
    (println (apply str (first rows)))
    (when-not (empty? (rest rows))
      (recur (rest rows)))))

(defn print-move [move]
  (println (serialize-move move)))
