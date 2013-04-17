
(defparameter *piece-color*
  (let ((new-piece-color
        (make-array 255 :initial-element nil)))
    (dolist (piece '(#\K #\Q #\B #\N #\R #\P))
      (setf (aref new-piece-color (char-int piece)) :white))
    (dolist (piece '(#\k #\q #\b #\n #\r #\p))
      (setf (aref new-piece-color (char-int piece)) :black))
    new-piece-color))

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

(defun in-bounds-p (coord)
  (and (>= (car coord) 0)
       (<= (car coord) 4)
       (>= (cdr coord) 0)
       (<= (cdr coord) 5)))

(defun piece-at (board coord)
  (let ((piece (char (aref board (cdr coord)) (car coord))))
    (when (not (eql #\. piece)) piece)))

;; TODO piece-colors
;; TODO opp-color
;; TODO piece-class
;; TODO empty-board
;; TODO initial-board
;; TODO initial-game-state
;; TODO coord-p
;; TODO move-p
;; TODO manhat
;; TODO in-bounds
;; TODO piece-at
;; TODO color-at
;; TODO coord-shift
;; TODO capture?
;; TODO valid-landing
;; TODO move-scan
;; TODO move-piece
;; TODO locations-of-color
