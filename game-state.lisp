(in-package :elo100)

;;(declaim (optimize speed))

;; (defun parse-state (state-sexp)
;;   (let* ((field 0)
;;          (board (getf state-sexp :board))
;;          (row (aref board 0))
;;          (row-offset 0))
;;     ;; :on-move
;;     (when (eql (getf state-sexp :on-move) :white)
;;       (setf (ldb (byte +move-size+ +move-offset+) field) 1))
;;     ;; :turn
;;     (setf (ldb (byte +turn-size+ +turn-offset+) field) (getf state-sexp :turn))
;;     ;; :board
;;     (loop for y from 0 to 5 do
;;          (setf row (aref board y))
;;          (setf row-offset (* +row-size+ y))
;;          (loop for x from 0 to 4 do
;;               (setf (ldb (byte +piece-size+ (+ +board-offset+
;;                                                row-offset
;;                                                (* x +piece-size+))) field) (piece-long-to-short (char row x)))))
;;     field))

(defstruct game-state
  (board-a nil)
  (board-b nil)
  (on-move +white+)
  (turn 0)
  (history))

;; (defparameter *clean-state*
;;   (parse-state '(:on-move :white
;;                  :turn 0
;;                  :board #("kqbnr"
;;                           "ppppp"
;;                           "....."
;;                           "....."
;;                           "PPPPP"
;;                           "RNBQK"))))

(defparameter *clean-state* (make-game-state))

(defparameter *game-state* *clean-state*)

;;(defun make-game-state () *game-state*)

;;(defmacro game-state-turn (state)
;;  `(ldb (byte +turn-size+ +turn-offset+) ,state))

;;(defmacro game-state-on-move (state)
;;  `(ldb (byte +move-size+ +move-offset+) ,state))

(defun piece-offset (x y)
  (+ +move-size+
     +turn-size+
     (* +row-size+ y)
     (* +piece-size+ x)))

(defun fast-piece-at (x y board-a board-b)
  (declare (type fixnum x y board-a board-b))
  (declare (optimize (safety 0)))
  ;;(declare (type fixnum actual-location))
  (let ((location (+ (* y 5) x)))
    (declare (type fixnum location))
    (if (> 15 location)
        (ldb (byte +piece-size+ (* +piece-size+ location)) board-a)
        (ldb (byte +piece-size+ (* +piece-size+ (- location 15))) board-b))))

(defun piece-at (state x y)
  (fast-piece-at x y (game-state-board-a state) (game-state-board-b state)))

(defun game-state-status (state possible-moves)
  ;;(declare (type (or cons nil) possible-moves))
  ;; Stealing whoppers way of checking for kings here :)
  (let ((white-king-alive nil)
        (black-king-alive nil)
        (white-king (color-piece +king+ +white+))
        (black-king (color-piece +king+ +black+)))
    (let* ((board-a (game-state-board-a state))
           (board-b (game-state-board-b state)))
      (loop for location from 0 to (* 14 +piece-size+) by +piece-size+ do
           (let ((piece-a (ldb (byte +piece-size+ location) board-a))
                 (piece-b (ldb (byte +piece-size+ location) board-b)))
             (cond
               ((or (eql piece-a white-king)
                    (eql piece-b white-king)) (setf white-king-alive T))
               ((or (eql piece-a black-king)
                    (eql piece-b black-king)) (setf black-king-alive T))))))
    (cond
      ((not white-king-alive) :black)
      ((not black-king-alive) :white)
      ((= 0 (length possible-moves)) (opp-color (game-state-on-move state)))
      ((> (game-state-turn state) 40) :draw)
      (T :ongoing))))

(defun game-state-update-piece (state x y piece)
  (let ((location (+ (* y 5) x)))
    (declare (type fixnum location))
    (if (> 15 location)
        (setf (ldb (byte +piece-size+ (* +piece-size+ location)) (game-state-board-a state)) piece)
        (setf (ldb (byte +piece-size+ (* +piece-size+ (- location 15))) (game-state-board-b state)) piece))))

(defun game-state-promote-pawns (state)
  (let* ((board-a (game-state-board-a state))
         (board-b (game-state-board-b state)))
    (dolist (y '(0 5))
      (loop for x from 0 to 4 do
           (let ((piece (fast-piece-at x y board-a board-b)))
             (when (= (piece-class piece) +pawn+)
               (game-state-update-piece state x y piece)))))))

(defun game-state-inc-turn (state)
  (setf (game-state-on-move state) (opp-color (game-state-on-move state)))
  (when (= (game-state-on-move state) +white+)
    (incf (game-state-turn state))))

(defun game-state-dec-turn (state)
  (setf (game-state-on-move state) (opp-color (game-state-on-move state)))
  (when (= (game-state-on-move state) +black+)
    (decf (game-state-turn state))))

(defun apply-move (state move)
  (let ((from-piece (piece-at state (x (from move)) (y (from move)))))
    (setf (game-state-history state) (cons (cons (game-state-board-a state)
                                                 (game-state-board-b state))
                                           (game-state-history state)))
    (game-state-update-piece state (x (from move)) (y (from move)) 0)
    (game-state-update-piece state (x (to move)) (y (to move)) from-piece)
    (game-state-promote-pawns state)
    (game-state-inc-turn state)))

(defparameter *move-application-cache-size* (expt 2 16))
(defparameter *move-application-cache* (make-array *move-application-cache-size* :element-type 'cons :initial-element nil))
(defparameter *move-application-cache-off* T)

(defun apply-move-cached (state move)
  (if *move-application-cache-off*
      (apply-move state move)
      (let* ((key (mod (sxhash (cons state move)) *move-application-cache-size*))
             (cached (aref *move-application-cache* key)))
        (if (and cached
                 (equal state (cadr cached))
                 (equal move (cddr cached)))
            (car cached)
            (let ((new-value (apply-move state move)))
              (setf (aref *move-application-cache* key) (cons new-value (cons state move)))
              new-value)))))

(defmacro with-state (state &body forms)
  `(let ((*game-state* ,state))
     ,@forms))
