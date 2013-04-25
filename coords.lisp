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

(defun manhat (coord-a coord-b)
  (+ (abs (- (x coord-a) (x coord-b)))
     (abs (- (y coord-a) (y coord-b)))))

(defun d-manhat (x-a y-a x-b y-b)
  (declare (type fixnum x-a y-a x-b y-b)
           (values fixnum))
  (+ (abs (- x-a x-b))
     (abs (- y-a y-b))))

