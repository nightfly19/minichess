(in-package :elo100)

(defmacro x (coord) `(car ,coord))

(defmacro y (coord) `(cdr ,coord))

(defmacro from (move) `(car ,move))

(defmacro to (move) `(cdr ,move))

(defmacro in-bounds-p (coord)
  `(and (>= (x ,coord) 0)
        (<= (x ,coord) 4)
        (>= (y ,coord) 0)
        (<= (y ,coord) 5)))

(defun d-in-bounds-p (x y)
  (declare (type fixnum x y))
  (and (>= x 0)
       (<= x 4)
       (>= y 0)
       (<= y 5)))

(defmacro raw-piece-at (board coord)
  `(char (aref ,board (y ,coord)) (x ,coord)))

(defun d-raw-piece-at (board x y)
  (char (aref board y) x))

(defun piece-at (board coord)
  (let ((piece (raw-piece-at board coord)))
    (when (not (eql #\. piece)) piece)))

(defun d-piece-at (board x y)
  (let ((piece (d-raw-piece-at board x y)))
    (when (not (eql #\. piece)) piece)))

(defun color-at (board coord)
  (piece-color (raw-piece-at board coord)))

(defun d-color-at (board x y)
  (piece-color (d-raw-piece-at board x y)))

(defun manhat (coord-a coord-b)
  (+ (abs (- (x coord-a) (x coord-b)))
     (abs (- (y coord-a) (y coord-b)))))

(defun d-manhat (x-a y-a x-b y-b)
  (declare (type fixnum x-a y-a x-b y-b)
           (values fixnum))
  (+ (abs (- x-a x-b))
     (abs (- y-a y-b))))

