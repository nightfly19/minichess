(ns minichess.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def x-upper 4)
(def y-upper 5)
(def max-turns 40)

(def all-directions (disj (into #{} (combo/cartesian-product (range -1 2) (range -1 2))) [0 0]))

(def piece-colors
  (let [w :white b :black]
    {\K w, \Q w, \B w, \N w, \R w, \P w,
     \k b, \q b, \b b, \n b, \r b, \p b}))

(def empty-board
  (vec (repeat 6 (vec (repeat 5 \.)))))

(def initial-board
  [(vec "kqbnr")
   (vec "ppppp")
   (vec ".....")
   (vec ".....")
   (vec "PPPPP")
   (vec "RNBQK")])

(def initial-game-state
  {:turn 0
   :on-move :white
   :board initial-board})

(def opp-color
  {:white :black
   :black :white})

(def piece-class
  {\R \R \N \N \B \B \Q \Q \K \K \P \P
   \r \R \n \N \b \B \q \Q \k \K \p \P})

(defn x [ coord] (nth coord 0))
(defn y [ coord] (nth coord 1))
(defn from [ move] (nth move 0))
(defn to [ move] (nth move 1))

(defmacro defnmemoized [name l-list & body]
  "Defines and memoizes a function in a single step"
  `(let [temp-fn# (fn ~l-list ~@body)]
     (defn ~name [& args#] (apply temp-fn# args#))))

(defnmemoized manhat [ coord-a  coord-b]
  "Computes the manhatten distance between two coordinates"
  (+ (math/abs (- (x coord-a) (x coord-b)))
     (math/abs (- (y coord-a) (y coord-b)))))

;;Warm up the manhat cache
(let [spaces (combo/cartesian-product (range 0 5) (range 0 6))]
  (doseq [[from to] (combo/cartesian-product spaces spaces)]
    (manhat (vec from) (vec to))))

(defnmemoized in-bounds [coord]
  (and (>= (x coord) 0) (<= (x coord) x-upper)
       (>= (y coord) 0) (<= (y coord) y-upper)))

(defn piece-at [board coord]
  (when (in-bounds coord)
    (let [piece (nth (nth board (y coord)) (x coord))]
      (if (not= piece \.) piece nil))))

(defn color-at [board coord]
  (piece-colors (piece-at board coord)))

(defn coord-shift [coord coord-d]
  [(+ (x coord) (x coord-d)) (+ (y coord) (y coord-d))])

(defn capture? [board color coord]
  (boolean (and (piece-at board coord)
                (not= color (color-at board coord)))))

(defn valid-landing
  ([board color coord]
     (valid-landing board color coord true))
  ([board color coord allow-capture]
     (let [piece (piece-at board coord)]
       (and (in-bounds coord)
            (not= color (color-at board coord))
            (or (not piece) allow-capture)))))

(defn move-scan
  ([board coord coord-d]
     (move-scan board coord coord-d true))

  ([board coord coord-d capture]
     (move-scan board coord coord-d capture (+ x-upper y-upper)))

  ([board coord coord-d capture max-manhat]
     (move-scan board coord coord-d (coord-shift coord coord-d) #{} capture max-manhat))

  ([board coord-0 coord-d coord moves capture max-manhat]
     (let [color (color-at board coord-0)
           piece (piece-at board coord)
           dist (manhat coord-0 coord)]
       (cond
        (> dist max-manhat) moves
        (not (in-bounds coord)) moves
        piece (cond
               (= (color-at board coord) color) moves
               (not capture) moves
               :default (conj moves [coord-0 coord]))
        :default (recur board coord-0 coord-d (coord-shift coord coord-d)
                        (conj moves [coord-0 coord]) capture max-manhat)))))

(defn update-location [board coord piece]
  (let [old-row (nth board (y coord))
        new-row (assoc old-row (x coord) piece)]
    (assoc board (y coord) new-row)))

(defn move-piece [board coord-a coord-b]
  (let [piece (piece-at board coord-a)]
    (-> board
        (update-location coord-a \.)
        (update-location coord-b piece))))

(let [spaces (combo/cartesian-product (range 0 (inc x-upper)) (range 0 (inc y-upper)))]
  (defn filter-board [board filter-fn]
    (filter filter-fn spaces)))

(defn locations-of-color [board color]
  (filter-board board (fn [location]
                        (= color (color-at board location)))))

(defn locations-of-piece [board desired-piece-class]
  (filter-board board (fn [location]
                        (= desired-piece-class (piece-class (piece-at board  location))))))

(defn filter-promotable-locations [locations]
  (filter (fn [location]
            (or (= (nth location 1) 0)
                (= (nth location 1) y-upper)))
          locations))

(defn- mover [action directions]
  (reduce set/union (map action directions)))

(defn promote-pawns [board]
  (let [pawns-to-promote
        (-> board (locations-of-piece \P) (filter-promotable-locations))]
    (reduce (fn [board pawn-loc]
              (let [color (color-at board pawn-loc)]
                (update-location board pawn-loc
                                 (if (= color :white)
                                   \Q \q))))
            board pawns-to-promote)))

;;Here for now because defmulti seems to be effectively defonced...
(def movelist 2)

(defmulti movelist (fn [board coord]
                     (piece-class (piece-at board coord))))

(defmethod movelist \N [board coord]
  (mover
   (fn [dir] (move-scan board coord dir true 3))
   #{[-1 2] [1 2] [2 1] [2 -1] [1 -2] [-1 -2] [-2 -1] [-2 1]}))

(defmethod movelist \Q [board coord]
  (mover
   (fn [dir] (move-scan board coord dir true))
   all-directions))

(defmethod movelist \K [board coord]
  (mover
   (fn [dir] (move-scan board coord dir true 1))
   all-directions))

(defmethod movelist \R [board coord]
  (mover
   (fn [dir] (move-scan board coord dir true))
   #{[1 0] [-1 0] [0 1] [0 -1] }))

(defmethod movelist \B [board coord]
  (reduce set/union #{}
          [(mover
            (fn [dir] (move-scan board coord dir true))
            #{[1 1] [-1 1] [1 -1] [-1 -1] })
           (mover
            (fn [dir] (move-scan board coord dir false))
            #{[0 1] [0 -1] [1 0] [-1 0] })]))

(defmethod movelist \P [board coord]
  (let [color (color-at board coord)
        normal-dirs (if (= color :white)
                      #{[0 -1]}
                      #{[0 1]})
        capture-dirs (if (= color :white)
                       #{[1 -1] [-1 -1]}
                       #{[1 1] [-1 1]})
        normal-moves (mover
                      (fn [dir] (move-scan board coord dir false 1))
                      normal-dirs)
        possible-captures (mover
                           (fn [dir] (move-scan board coord dir true 2))
                           capture-dirs)
        actual-captures (filter (fn [pos-capture]
                                  (let [target (nth pos-capture 1)]
                                    (capture? board color target)))
                                possible-captures)]
    (reduce set/union #{} [normal-moves actual-captures])))

(defmethod movelist :default [board coord] #{})

(defn moves-for-color [board color]
  (let [pieces (locations-of-color board color)]
    (into #{} (reduce concat
                      (map (fn [piece]
                             (movelist board piece))
                           pieces)))))

(defnmemoized possible-moves [state]
  "Returns a set of all legal moves for state (memoized)"
  (moves-for-color (:board state) (:on-move state)))

(defnmemoized move-legal? [state move]
  "Is move legal for state (memoized)"
  (contains? (possible-moves state) move))

(defnmemoized game-status [state]
  "computes the current status of the game (memoized"
  (let [kings (locations-of-piece (:board state) \K)]
    (cond
     (<= 40 (:turn state)) :draw
     (= 0 (count (possible-moves state))) (opp-color (:on-move state))
     (= 1 (count kings)) (color-at (:board state) (first kings))
     :default :ongoing)))

(defn apply-move [state move]
  "returns a new state with the move given applied"
  (-> state
      (#(assoc %1 :board (move-piece (:board %1) (from move) (to move))))
      (#(assoc %1 :board (promote-pawns (:board %1))))
      (#(if (= :black (:on-move %1))
          (assoc %1 :turn (inc (:turn %1)))
          %1))
      (#(assoc %1 :on-move (opp-color (:on-move %1))))))

(defnmemoized possible-states [state]
  (reduce (fn [states move] (assoc states move (apply-move state move))) {} (possible-moves state)))

(defn game-over? [state]
  (not= :ongoing (game-status state)))
