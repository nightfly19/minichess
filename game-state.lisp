(in-package :elo100)

(declaim (optimize speed))

(defun parse-state (state-sexp)
  (let* ((field 0)
         (board (getf state-sexp :board))
         (row (aref board 0))
         (row-offset 0))
    ;; :on-move
    (when (eql (getf state-sexp :on-move) :white)
      (setf (ldb (byte +move-size+ +move-offset+) field) 1))
    ;; :turn
    (setf (ldb (byte +turn-size+ +turn-offset+) field) (getf state-sexp :turn))
    ;; :board
    (loop for y from 0 to 5 do
         (setf row (aref board y))
         (setf row-offset (* +row-size+ y))
         (loop for x from 0 to 4 do
              (setf (ldb (byte +piece-size+ (+ +board-offset+
                                               row-offset
                                               (* x +piece-size+))) field) (piece-long-to-short (char row x)))))
    field))

(defparameter *clean-state*
  (parse-state '(:on-move :white
                 :turn 0
                 :board #("kqbnr"
                          "ppppp"
                          "....."
                          "....."
                          "PPPPP"
                          "RNBQK"))))

(defparameter *game-state* *clean-state*)

(defun make-game-state () *clean-state*)

(defun make-initial-game-state () *clean-state*)

(defmacro game-state-turn (state)
  `(ldb (byte +turn-size+ +turn-offset+) ,state))

(defmacro game-state-on-move (state)
  `(ldb (byte +move-size+ +move-offset+) ,state))

(defun piece-offset (x y)
  (+ +move-size+
     +turn-size+
     (* +row-size+ y)
     (* +piece-size+ x)))

(defmacro piece-at (state x y)
  `(ldb (byte +piece-size+ (piece-offset ,x ,y)) ,state))

(defun fast-piece-at (x y board-a board-b)
  (declare (type fixnum x y board-a board-b))
  (declare (optimize (safety 0)))
  ;;(declare (type fixnum actual-location))
  (let ((location (+ (* y 5) x)))
    (declare (type fixnum location))
    (if (> 15 location)
        (ldb (byte +piece-size+ (* +piece-size+ location)) board-a)
        (ldb (byte +piece-size+ (* +piece-size+ (- location 15))) board-b))))

(defun game-state-status (state possible-moves)
  ;;(declare (type (or cons nil) possible-moves))
  ;; Stealing whoppers way of checking for kings here :)
  (let ((white-king-alive nil)
        (black-king-alive nil)
        (white-king (color-piece +king+ +white+))
        (black-king (color-piece +king+ +black+)))
    (let* ((half-board-size (* 15 +piece-size+))
           (board-a (ldb (byte half-board-size +board-offset+) state))
           (board-b (ldb (byte half-board-size (+ +board-offset+
                                                  half-board-size)) state)))
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
  (dpb piece (byte +piece-size+ (piece-offset x y)) state))

(defun game-state-promote-pawns (state)
  (let* ((half-board-size (* 15 +piece-size+))
         (board-a (ldb (byte half-board-size +board-offset+) state))
         (board-b (ldb (byte half-board-size (+ +board-offset+
                                                half-board-size)) state))
         (new-state state))
    (dolist (y '(0 5))
      (loop for x from 0 to 4 do
           (let ((piece (fast-piece-at x y board-a board-b)))
             (when (= (piece-class piece) +pawn+)
               (setf (ldb (byte 3 (+ 1 (piece-offset x y))) new-state) +queen+)))))
    new-state))

(defun game-state-inc-turn (state)
  (let* ((new-state (dpb (opp-color (game-state-on-move state)) (byte +move-size+ +move-offset+) state))
         (color (game-state-on-move new-state)))
    (if (= color +white+)
        (dpb (+ (game-state-turn state) 1) (byte +turn-size+ +turn-offset+) new-state)
        new-state)))

(defun apply-move (state move)
  (let ((from-piece (piece-at state (x (from move)) (y (from move)))))
    (game-state-inc-turn
     (game-state-promote-pawns
      (game-state-update-piece
       (game-state-update-piece state (x (from move)) (y (from move)) 0)
       (x (to move)) (y (to move)) from-piece)))))

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
