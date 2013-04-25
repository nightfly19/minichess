(in-package :elo100)

(declaim (optimize speed))

(defconstant +move-size+ 1)
(defconstant +move-offset+ 0)
(defconstant +turn-offset+ 1)
(defconstant +turn-size+ 6)
(defconstant +board-offset+ 7)
(defconstant +piece-size+ (integer-length 13))
(defconstant +row-size+ (* +piece-size+ 5))

(defparameter *piece-color*
  (let ((new-piece-color
         (make-array 255 :initial-element nil :element-type '(or null keyword))))
    (dolist (piece '(#\K #\Q #\B #\N #\R #\P))
      (setf (aref new-piece-color (char-int piece)) :white))
    (dolist (piece '(#\k #\q #\b #\n #\r #\p))
      (setf (aref new-piece-color (char-int piece)) :black))
    new-piece-color))

(defparameter *piece-value*
  (let ((new-piece-value
         (make-array 255 :initial-element 0 :element-type 'fixnum)))
    (dolist (piece '((#\K #\k 20000) (#\Q #\q 900) (#\R #\r 500) (#\N #\n 300) (#\B #\b 300) (#\P #\p 100)))
      (setf (aref new-piece-value (char-int (nth 0 piece))) (nth 2 piece))
      (setf (aref new-piece-value (char-int (nth 1 piece))) (- (nth 2 piece))))
    new-piece-value)
  "Warning!!! This is non intuative!!!")

(defparameter *piece-class*
  (let ((new-piece-class
         (make-array 255 :initial-element nil)))
    (dolist (piece '((#\K . #\k) (#\Q . #\q) (#\B . #\b) (#\N . #\n) (#\R . #\r) (#\P . #\p)))
      (setf (aref new-piece-class (char-int (car piece))) (car piece))
      (setf (aref new-piece-class (char-int (cdr piece))) (car piece)))
    new-piece-class))

(defun piece-color (piece)
  (declare (type simple-vector *piece-color*)
           (values (or null keyword)))
  (aref *piece-color* (char-int piece)))

(defun piece-class (piece)
  (declare (type simple-vector *piece-class*)
           (values standard-char))
  (aref *piece-class* (char-int piece)))

(defun piece-value (piece)
  "Warning!!! This is non-intuative"
  (declare (type (simple-array fixnum) *piece-value*)
           (values fixnum))
  (aref *piece-value* (char-int piece)))


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

