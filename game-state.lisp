(in-package :elo100)

(defun make-board ()
  (copy-seq `#(,(copy-seq "kqbnr")
               ,(copy-seq "ppppp")
               ,(copy-seq ".....")
               ,(copy-seq ".....")
               ,(copy-seq "PPPPP")
               ,(copy-seq "RNBQK"))))

(defstruct (game-state)
  (board (make-board))
  (turn 0)
  (on-move :white)
  (history nil))

(defparameter *game-state* (make-game-state))

(defun opp-color (color)
  (if (eql color :white) :black :white))

(defun game-state-status (state possible-moves)
  (declare (type cons possible-moves))
  ;; Stealing whoppers way of checking for kings here :)
  (let ((white-king nil)
        (black-king nil)
        (board (game-state-board state)))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let ((piece (piece-at board (cons x y))))
                (cond
                  ((eql piece #\K) (setf white-king T))
                  ((eql piece #\k) (setf black-king T))))))
    (cond
      ((not white-king) :black)
      ((not black-king) :white)
      ((= 0 (length possible-moves)) (opp-color (game-state-on-move state)))
      ((> (game-state-turn state) 40) :draw)
      (T :ongoing))))


(defmacro update-location-raw (board coord piece)
  `(setf (char (aref ,board (y ,coord)) (x ,coord)) ,piece))

(defun update-location (board coord piece history)
  (let ((old-piece (raw-piece-at board coord)))
    (setf (char (aref board (y coord)) (x coord)) piece)
    (cons (cons coord old-piece) history)))

(defun update-row (row x y piece history)
  (let ((old-piece (char row x)))
    (setf (char row x) piece)
    (cons (cons (cons x y) old-piece) history)))

(defun promote-pawns (board history)
  (declare (type (simple-vector 6) board))
  (let ((history history))
    (dolist (y '(0 5))
      (let ((row (aref board y)))
        (loop for x from 0 to 4 do
             (let ((piece (char row x)))
               (cond
                 ((eql piece #\p) (setf history (update-row row x y #\q history)))
                 ((eql piece #\P) (setf history (update-row row x y #\P history))))))))
    history))

(defun apply-move (move)
  (let* ((state *game-state*)
         (board (game-state-board state))
         (history nil)
         (from-piece (raw-piece-at board (from move))))
    (setf history (update-location board (from move) #\. history))
    (setf history (update-location board (to move) from-piece history))
    (setf history (promote-pawns board history))
    (setf (game-state-history state) (cons history (game-state-history state)))
    (setf (game-state-on-move state) (opp-color (game-state-on-move state)))
    (when (eql (game-state-on-move state) :white)
      (setf (game-state-turn state) (+ 1 (game-state-turn state))))
    state))

(defun undo-move ()
  (let* ((state *game-state*)
         (board (game-state-board state))
         (history (car (game-state-history state))))
    (when history
      (setf (game-state-on-move state) (opp-color (game-state-on-move state)))
      (dolist (old-move history)
        (update-location-raw board (car old-move) (cdr old-move)))
      (when (eql (game-state-on-move state) :black)
        (setf (game-state-turn state) (- (game-state-turn state) 1)))
      (setf (game-state-history state) (cdr (game-state-history state)))
      state)))

(defun state-to-integer (state)
  (let* ((field 0)
         (board (game-state-board state))
         (row (aref board 0))
         (row-offset 0))
    ;; :on-move
    (when (eql (game-state-on-move state) :white)
      (setf (ldb (byte +move-size+ +move-offset+) field) 1))
    ;; :turn
    (setf (ldb (byte +turn-size+ +turn-offset+) field) (game-state-turn state))
    ;; :board
    (loop for y from 0 to 5 do
         (setf row (aref board y))
         (setf row-offset (* +row-size+ y))
         (loop for x from 0 to 4 do
              (setf (ldb (byte +piece-size+ (+ +board-offset+
                                               row-offset
                                               (* x +piece-size+))) field) (piece-minimal-value (char row x)))))
    field))
