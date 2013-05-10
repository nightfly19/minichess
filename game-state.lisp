(in-package :elo100)

(defparameter *zoberist-array* (make-array (list 30 (expt 2 +piece-size+)) :element-type 'fixnum))

(loop for i from 0 upto (- 30 1) do
     (loop for g from 0 to (- (expt 2 +piece-size+) 1) do
          (setf (aref *zoberist-array* i g) (if (< 0 (random 2)) (random most-positive-fixnum)
                                                (- (random (abs most-negative-fixnum)))))))
(defun zoberist-piece-value (x y piece)
  (aref *zoberist-array* (+ (* y 5) x) piece))

(defun zoberist-get-value (state x y)
  (aref *zoberist-array* (+ (* y 5) x) (piece-at state x y)))

(defun zoberist-get-value-fast (board-a board-b x y)
  (aref *zoberist-array* (+ (* y 5) x) (fast-piece-at x y board-a board-b)))

(defun zoberist-hash-state (state)
  (let ((board-a (game-state-board-a state))
        (board-b (game-state-board-b state))
        (hash 0))
    (loop for x from 0 to 4 do
         (loop for y from 0 to 5 do
              (setf hash (logxor hash (zoberist-get-value-fast board-a board-b x y)))))
    hash))

;;(declaim (optimize speed))

(defstruct game-state
  (board-a 0)
  (board-b 0)
  (on-move +white+)
  (turn 0)
  (hash 0)
  (history nil))

(defstruct game-state-history
  (board-a)
  (board-b)
  (hash)
  (history))

(defun game-state-history-copy (history)
  (when history
    (make-game-state-history
                   :board-a (game-state-history-board-a history)
                   :board-b (game-state-history-board-b history)
                   :hash (game-state-history-hash history)
                   :history (game-state-history-copy (game-state-history-history history)))))

(defun game-state-copy (state)
  (when state
    (make-game-state
     :board-a (game-state-board-a state)
     :board-b (game-state-board-b state)
     :on-move (game-state-on-move state)
     :turn (game-state-turn state)
     :hash (game-state-hash state)
     :history (game-state-history-copy (game-state-history state)))))

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
      ((not white-king-alive) +black+)
      ((not black-king-alive) +white+)
      ((= 0 (length possible-moves)) (opp-color (game-state-on-move state)))
      ((> (game-state-turn state) 40) :draw)
      (T :ongoing))))

(defun game-state-update-piece (state x y piece)
  (let ((location (+ (* y 5) x)))
    (declare (type fixnum location))
    (setf (game-state-hash state) (logxor (game-state-hash state) (zoberist-piece-value x y piece)))
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

(defun game-state-apply-move (state move)
  (let ((from-piece (piece-at state (x (from move)) (y (from move)))))
    (setf (game-state-history state) (make-game-state-history :board-a (game-state-board-a state)
                                                              :board-b (game-state-board-b state)
                                                              :hash (game-state-hash state)
                                                              :history (game-state-history state)))
    (game-state-update-piece state (x (from move)) (y (from move)) 0)
    (game-state-update-piece state (x (to move)) (y (to move)) from-piece)
    (setf (game-state-hash state) (- (game-state-hash state)))
    (game-state-promote-pawns state)
    (game-state-inc-turn state))
  state)

(defun game-state-undo-move (state)
  (let ((history (game-state-history state)))
    (if history
        (progn
          (setf (game-state-board-a state) (game-state-history-board-a history))
          (setf (game-state-board-b state) (game-state-history-board-b history))
          (setf (game-state-hash state) (game-state-history-hash history))
          (game-state-dec-turn state)
          (setf (game-state-history state) (game-state-history-history history))
          state)
        state)))

(defun parse-state (state-sexp)
  (let* ((board (getf state-sexp :board))
         (row (aref board 0))
         (row-offset 0)
         (state (make-game-state)))
    ;; :on-move
    (if (eql (getf state-sexp :on-move) :white)
        (setf (game-state-on-move state) +white+)
        (setf (game-state-on-move state) +black+))
    ;; :turn
    (setf (game-state-turn state) (getf state-sexp :turn))
    ;; :board
    (loop for y from 0 to 5 do
         (setf row (aref board y))
         (setf row-offset (* +row-size+ y))
         (loop for x from 0 to 4 do
              (game-state-update-piece state x y (piece-long-to-short (char row x)))))
    (setf (game-state-hash state) (zoberist-hash-state state))
    state))

(defparameter *clean-state*
  (parse-state '(:on-move :white
                 :turn 0
                 :board #("kqbnr"
                          "ppppp"
                          "....."
                          "....."
                          "PPPPP"
                          "RNBQK"))))

(defun make-initial-game-state ()
  (make-game-state
   :board-a (game-state-board-a *clean-state*)
   :board-b (game-state-board-b *clean-state*)
   :hash (game-state-hash *clean-state*)))

(defparameter *game-state* (make-initial-game-state))

;; (defparameter *move-application-cache-size* (expt 2 16))
;; (defparameter *move-application-cache* (make-array *move-application-cache-size* :element-type 'cons :initial-element nil))
;; (defparameter *move-application-cache-off* T)

;; (defun apply-move-cached (state move)
;;   (if *move-application-cache-off*
;;       (apply-move state move)
;;       (let* ((key (mod (sxhash (cons state move)) *move-application-cache-size*))
;;              (cached (aref *move-application-cache* key)))
;;         (if (and cached
;;                  (equal state (cadr cached))
;;                  (equal move (cddr cached)))
;;             (car cached)
;;             (let ((new-value (apply-move state move)))
;;               (setf (aref *move-application-cache* key) (cons new-value (cons state move)))
;;               new-value)))))

(defmacro with-state (state &body forms)
  `(let ((*game-state* ,state))
     ,@forms))
