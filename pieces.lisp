(in-package :elo100)

(declaim (optimize speed))

(defconstant +move-size+ 1)
(defconstant +move-offset+ 0)
(defconstant +turn-offset+ 1)
(defconstant +turn-size+ 6)
(defconstant +board-offset+ 7)
(defconstant +piece-size+ (integer-length 13))
(defconstant +row-size+ (* +piece-size+ 5))

(defconstant +clear+ 0)
(defconstant +pawn+ 1)
(defconstant +bishop+ 2)
(defconstant +knight+ 3)
(defconstant +rook+  4)
(defconstant +queen+ 5)
(defconstant +king+ 6)

(defconstant +white+ 1)
(defconstant +black+ 0)

(defparameter *piece-value*
  (make-array 7
              :element-type 'fixnum
              :initial-contents '(0 100 300 300 500 900 30000)))

(declaim (type (simple-array fixnum (7)) *piece-value*))

(defmacro piece-color (piece)
  `(ldb (byte 1 0) ,piece))

(defmacro piece-class (piece)
  `(ldb (byte 3 1) ,piece))

(defmacro piece-value (piece)
  `(aref *piece-value* (piece-class ,piece)))

(defmacro color-piece (piece color)
  "returns piece of the given color"
  `(logior (ash ,piece 1) ,color))

(defparameter *piece-long-to-short*
  (let ((table (make-array 255 :element-type 'fixnum :initial-element 0)))
    (setf (aref table (char-int #\.)) 0)
    (setf (aref table (char-int #\p)) (color-piece +pawn+ +black+))
    (setf (aref table (char-int #\P)) (color-piece +pawn+ +white+))
    (setf (aref table (char-int #\r)) (color-piece +rook+ +black+))
    (setf (aref table (char-int #\R)) (color-piece +rook+ +white+))
    (setf (aref table (char-int #\n)) (color-piece +knight+ +black+))
    (setf (aref table (char-int #\N)) (color-piece +knight+ +white+))
    (setf (aref table (char-int #\b)) (color-piece +bishop+ +black+))
    (setf (aref table (char-int #\B)) (color-piece +bishop+ +white+))
    (setf (aref table (char-int #\q)) (color-piece +queen+ +black+))
    (setf (aref table (char-int #\Q)) (color-piece +queen+ +white+))
    (setf (aref table (char-int #\k)) (color-piece +king+ +black+))
    (setf (aref table (char-int #\K)) (color-piece +king+ +white+))
    table))

(declaim (type (simple-array fixnum (255)) *piece-long-to-short*))

(defmacro piece-long-to-short (piece)
  `(aref *piece-long-to-short* (char-int ,piece)))

(defmacro opp-color (color)
  `(if (eql ,color +white+) +black+ +white+))
