(in-package :elo100)

(defparameter *heuristic* (constantly 0))
(defparameter *game-status* nil)

(defun game-status (state possible-moves)
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

(defstruct (game-status
             (:constructor make-game-status
                           (&key possible-moves
                                 (status (game-status *game-state* possible-moves))
                                 (score (funcall *heuristic* *game-state* status)))))
  (possible-moves (possible-moves))
  (score (funcall *heuristic* *game-state*))
  (status (cows)))

