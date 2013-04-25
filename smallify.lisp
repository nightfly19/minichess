(in-package :elo100)

(defparameter *piece-minimal-value*
  (let ((new-piece-value
         (make-array 255 :initial-element 0 :element-type 'fixnum))
        (current-value 0))
    (dolist (piece '(#\K #\k #\Q #\q #\R #\r #\N #\n #\B #\b #\P #\p))
      (setf (aref new-piece-value (char-int piece)) current-value)
      (setf current-value (+ current-value 1)))
    new-piece-value)
  "Warning!!! This is non intuative!!!")

(defparameter *minimal-value-piece*
  (let ((new-value-piece
         (make-array 255 :initial-element #\. :element-type 'character)))
    new-value-piece))

(defmacro piece-minimal-value (piece)
  `(aref *piece-minimal-value* (char-int ,piece)))

(defconstant +move-size+ 1)
(defconstant +move-offset+ 0)
(defconstant +turn-offset+ 1)
(defconstant +turn-size+ 6)
(defconstant +board-offset+ 7)
(defconstant +piece-size+ (integer-length 13))
(defconstant +row-size+ (* +piece-size+ 5))

(defun state-to-integer (state)
  (let* ((field 0)
         (board (getf state :board))
         (row (aref board 0))
         (row-offset 0))
    ;; :on-move
    (when (eql (getf state :on-move) :white)
      (setf (ldb (byte +move-size+ +move-offset+) field) 1))
    ;; :turn
    (setf (ldb (byte +turn-size+ +turn-offset+) field) (getf state :turn))
    ;; :board
    (loop for y from 0 to 5 do
         (setf row (aref board y))
         (setf row-offset (* +row-size+ y))
         (loop for x from 0 to 4 do
              (setf (ldb (byte +piece-size+ (+ +board-offset+
                                               row-offset
                                               (* x +piece-size+))) field) (piece-minimal-value (char row x)))))
    field))

;;(defun integer-to-state (int-state &optional (state (make-state)))
;;  state)
