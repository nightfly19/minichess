(in-package :elo100)

(defparameter *quick-movescan-mode* nil)

;;(declaim (optimize (speed 0)
;;                   (debug 3)))

(defun move-scan (moves board-a board-b color x y coord-d capture require-capture max-manhat)
  (let ((keep-searching T)
        (moves moves)
        (o-x x)
        (o-y y)
        (cur-x x)
        (cur-y y)
        (d-x (x coord-d))
        (d-y (y coord-d)))
    (declare (type fixnum cur-x d-x cur-y d-y o-x o-y max-manhat))
    (setf cur-x (+ cur-x d-x))
    (setf cur-y (+ cur-y d-y))
    (loop while (and keep-searching
                     (not (and *quick-movescan-mode* moves))
                     (d-in-bounds-p cur-x cur-y)
                     (<= (d-manhat o-x o-y cur-x cur-y) max-manhat)) do
         (let ((piece (fast-piece-at cur-x cur-y board-a board-b)))
           (if (not (= piece 0))
               (progn
                 (cond
                   ((not capture)
                    (setf keep-searching nil))
                   ((eql color (piece-color piece))
                    (setf keep-searching nil))
                   (T (setf keep-searching nil)
                      (setf moves (cons (cons (cons x y) (cons cur-x cur-y)) moves)))))
               (when (not require-capture)
                 (setf moves (cons (cons (cons x y) (cons cur-x cur-y)) moves)))))
         (setf cur-x (+ cur-x d-x))
         (setf cur-y (+ cur-y d-y)))
    moves))

(defun mover (action directions moves)
  (reduce action directions :initial-value moves))

(defgeneric inner-move-list (board-a board-b color x y moves piece-class))

(defmethod inner-move-list (board-a board-b color x y moves piece-class)
  moves)

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +knight+)))
  (mover (lambda (moves direction)
           (move-scan moves board-a board-b color x y direction T nil 3))
         '((-1 . 2) (1 . 2)   (2 . 1)   (2 . -1)
           (1 . -2) (-1 . -2) (-2 . -1) (-2 . 1)) moves))

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +rook+)))
  (mover (lambda (moves direction)
           (move-scan moves board-a board-b color x y direction T nil 99))
         '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +queen+)))
  (mover (lambda (moves direction)
           (move-scan moves board-a board-b color x y direction T nil 99))
         '((-1 . 1) (0 . 1) (1 . 1)
           (-1 . 0)         (1 . 0)
           (-1 . -1)(0 . -1)(1 . -1)) moves))

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +king+)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board-a board-b color x y direction T nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    (when (and *quick-movescan-mode* moves) (return-from inner-move-list moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board-a board-b color x y direction T nil 2))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    moves))

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +bishop+)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board-a board-b color x y direction T nil 99))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    (when (and *quick-movescan-mode* moves) (return-from inner-move-list moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board-a board-b color x y direction nil nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    moves))

(defmethod inner-move-list (board-a board-b color x y moves (piece-class (eql +pawn+)))
  (let ((color (piece-color (fast-piece-at x y board-a board-b)))
        (moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board-a board-b color x y direction T T 2))
                       (if (eql color +white+)
                           '((1 . -1) (-1 . -1))
                           '((1 . 1) (-1 . 1))) moves))
    (when (and *quick-movescan-mode* moves) (return-from inner-move-list moves))
    (mover (lambda (moves direction)
             (move-scan moves board-a board-b color x y direction nil nil 1))
           (if (eql color +white+) '((0 . -1)) '((0 . 1))) moves)))

(defun move-list (board-a board-b spot-color x y moves)
  (inner-move-list board-a board-b spot-color x y moves (piece-class (fast-piece-at x y board-a board-b))))

(defun possible-moves (state &optional quick-mode)
  (let* ((*quick-movescan-mode* quick-mode)
         (color (game-state-on-move state))
         (board-a (game-state-board-a state))
         (board-b (game-state-board-b state))
         (moves ()))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let* ((piece (fast-piece-at x y board-a board-b))
                     (spot-color (piece-color piece)))
                (when (and (not (= piece 0)) (eql color spot-color))
                  (setf moves (move-list board-a board-b spot-color x y moves))
                  (when (and *quick-movescan-mode* moves) (return-from possible-moves moves))))))
    moves))
