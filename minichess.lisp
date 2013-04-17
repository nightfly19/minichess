
(defparameter *piece-color*
  (let ((new-piece-color
         (make-array 255 :initial-element nil)))
    (dolist (piece '(#\K #\Q #\B #\N #\R #\P))
      (setf (aref new-piece-color (char-int piece)) :white))
    (dolist (piece '(#\k #\q #\b #\n #\r #\p))
      (setf (aref new-piece-color (char-int piece)) :black))
    new-piece-color))

(defparameter *piece-class*
  (let ((new-piece-class
         (make-array 255 :initial-element nil)))
    (dolist (piece '((#\K . #\k) (#\Q . #\a) (#\B . #\b) (#\N . #\n) (#\R . #\r) (#\P . #\p)))
      (setf (aref new-piece-class (char-int (car piece))) (car piece))
      (setf (aref new-piece-class (char-int (cdr piece))) (car piece)))
    new-piece-class))

(defparameter *initial-board*
  #("kqbnr"
    "ppppp"
    "....."
    "....."
    "PPPPP"
    "RNBQK"))

(defparameter *initial-state*
  `(:board ,*initial-board*
           :on-move :white
           :turn 0
           :history ()))

(defmacro piece-color (piece)
  `(when ,piece (aref *piece-color* (char-int ,piece))))

(defmacro piece-class (piece)
  `(when ,piece (aref *piece-class* (char-int ,piece))))

(defun in-bounds-p (coord)
  (and (>= (car coord) 0)
       (<= (car coord) 4)
       (>= (cdr coord) 0)
       (<= (cdr coord) 5)))

(defun piece-at (board coord)
  (let ((piece (char (aref board (cdr coord)) (car coord))))
    (when (not (eql #\. piece)) piece)))

(defun color-at (board coord)
  (piece-color (piece-at board coord)))

(defun manhat (coord-a coord-b)
  (+ (abs (- (car coord-a) (car coord-b)))
     (abs (- (cdr coord-a) (cdr coord-b)))))

(defun opp-color (color)
  (if (eql color :white) :black :white))

(defun add-coord (coord-a coord-b)
  (cons (+ (car coord-a) (car coord-b))
        (+ (cdr coord-a) (cdr coord-b))))

(defun valid-landing-p (board color coord allow-capture)
  (when (in-bounds-p coord)
    (let ((piece (piece-at board coord)))
      (cond
        ((and piece
              allow-capture
              (not (eql color (piece-color piece)))) T)
        (piece NIL)
        (T T)))))

(defun capture-p (board color coord)
  "might want to redo this"
  (not (and (valid-landing-p board color coord T)
            (valid-landing-p board color coord nil))))

(defun move-scan (board coord coord-d capture require-capture max-manhat)
  (let ((moves ())
        (cur-coord (add-coord coord coord-d))
        (color (color-at board coord)))
    (loop while (and (in-bounds-p cur-coord)
                     (<= (manhat coord cur-coord) max-manhat)) do
         (let ((piece (piece-at board cur-coord)))
           (if piece
               (cond
                 ((not capture) (return-from move-scan moves))
                 ((eql color (piece-color piece)) (return-from move-scan moves))
                 (T (return (cons (cons coord cur-coord) moves))))
               (when (not require-capture)
                 (setf moves (cons (cons coord cur-coord) moves))))
           (setf cur-coord (add-coord cur-coord coord-d))))
    moves))

;; TODO move-piece
;; TODO locations-of-color
