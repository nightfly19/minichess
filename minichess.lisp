
(load "utility.lisp")

(defmacro time-code (&body body)
  (let ((start-time (gensym "start"))
        (output (gensym "output"))
        (end-time (gensym "end")))
    `(let* ((,start-time (get-internal-real-time))
            (,output (progn ,@body))
            (,end-time (get-internal-real-time)))
       (format T "~A~%" (/ (float (- ,end-time ,start-time))
                           internal-time-units-per-second))
       ,output)))

(defparameter *default-cache-size* (expt 2 24))

(defparameter *piece-color*
  (let ((new-piece-color
         (make-array 255 :initial-element nil)))
    (dolist (piece '(#\K #\Q #\B #\N #\R #\P))
      (setf (aref new-piece-color (char-int piece)) :white))
    (dolist (piece '(#\k #\q #\b #\n #\r #\p))
      (setf (aref new-piece-color (char-int piece)) :black))
    new-piece-color))

(defparameter *piece-value*
  (let ((new-piece-value
         (make-array 255 :initial-element 0)))
    (dolist (piece '((#\K #\k 10000) (#\Q #\q 900) (#\R #\r 500) (#\N #\n 300) (#\B #\b 300) (#\P #\p 100)))
      (setf (aref new-piece-value (char-int (nth 0 piece))) (nth 2 piece))
      (setf (aref new-piece-value (char-int (nth 1 piece))) (-(nth 2 piece))))
    new-piece-value)
  "Warning!!! This is non intuative!!!")

(defparameter *piece-class*
  (let ((new-piece-class
         (make-array 255 :initial-element nil)))
    (dolist (piece '((#\K . #\k) (#\Q . #\q) (#\B . #\b) (#\N . #\n) (#\R . #\r) (#\P . #\p)))
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

(defmacro x (coord) `(car ,coord))

(defmacro y (coord) `(cdr ,coord))

(defmacro from (move) `(car ,move))

(defmacro to (move) `(cdr ,move))

(defmacro piece-color (piece)
  `(when ,piece (aref *piece-color* (char-int ,piece))))

(defmacro piece-class (piece)
  `(when ,piece (aref *piece-class* (char-int ,piece))))

(defmacro piece-value (piece)
  "Warning!!! This is non-intuative"
  `(aref *piece-value* (char-int ,piece)))

(defun in-bounds-p (coord)
  (and (>= (car coord) 0)
       (<= (car coord) 4)
       (>= (cdr coord) 0)
       (<= (cdr coord) 5)))

(defun raw-piece-at (board coord)
  (char (aref board (cdr coord)) (car coord)))

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

(defun move-scan (moves board coord coord-d capture require-capture max-manhat)
  (let ((cur-coord (add-coord coord coord-d))
        (color (color-at board coord)))
    (loop while (and (in-bounds-p cur-coord)
                     (<= (manhat coord cur-coord) max-manhat)) do
         (let ((piece (piece-at board cur-coord)))
           (if piece
               (cond
                 ((not capture) (return-from move-scan moves))
                 ((eql color (piece-color piece)) (return-from move-scan moves))
                 (T (return-from move-scan (cons (cons coord cur-coord) moves))))
               (when (not require-capture)
                 (setf moves (cons (cons coord cur-coord) moves))))
           (setf cur-coord (add-coord cur-coord coord-d))))
    moves))

(defun mover (action directions moves)
  (reduce action directions :initial-value moves))

(defgeneric inner-move-list (board coord moves piece-class))
(defmethod inner-move-list (board coord moves piece-class) moves)

(defmethod inner-move-list (board coord moves (piece-class (eql #\N)))
  (mover (lambda (moves direction)
           (move-scan moves board coord direction T nil 3))
         '((-1 . 2) (1 . 2)   (2 . 1)   (2 . -1)
           (1 . -2) (-1 . -2) (-2 . -1) (-2 . 1)) moves))

(defmethod inner-move-list (board coord moves (piece-class (eql #\R)))
  (mover (lambda (moves direction)
           (move-scan moves board coord direction T nil 99))
         '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))

(defmethod inner-move-list (board coord moves (piece-class (eql #\Q)))
  (mover (lambda (moves direction)
           (move-scan moves board coord direction T nil 99))
         '((-1 . 1) (0 . 1) (1 . 1)
           (-1 . 0)         (1 . 0)
           (-1 . -1)(0 . -1)(1 . -1)) moves))

(defmethod inner-move-list (board coord moves (piece-class (eql #\K)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction T nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction T nil 2))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    moves))

(defmethod inner-move-list (board coord moves (piece-class (eql #\B)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction T nil 99))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction nil nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    moves))

(defmethod inner-move-list (board coord moves (piece-class (eql #\P)))
  (let ((color (color-at board coord))
        (moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction nil nil 1))
                       (if (eql color :white) '((0 . -1)) '((0 . 1))) moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board coord direction nil nil 1))
                       (if (eql color :white)
                           '((1 . -1) (-1 . -1))
                           '((1 . 1) (-1 . 1))) moves))
    moves))

(defun move-list (board coord moves)
  (inner-move-list board coord moves (piece-class (piece-at board coord))))

(defun possible-moves (state)
  (let ((board (getf state :board))
        (color (getf state :on-move))
        (moves ()))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let* ((coord (cons x y))
                     (spot-color (color-at board coord)))
                (when (eql color spot-color)
                  (setf moves (move-list board coord moves))))))
    moves))

(defun update-location-raw (board coord piece)
  (setf (char (aref board (y coord)) (x coord)) piece))

(defun update-location (board coord piece history)
  (let ((old-piece (raw-piece-at board coord)))
    (setf (char (aref board (y coord)) (x coord)) piece)
    (cons (cons coord old-piece) history)))

(defun promote-pawns (board history) history)

(defun apply-move (state move)
  (let* ((board (getf state :board))
         (history nil)
         (from-piece (raw-piece-at board (from move))))
    (setf history (update-location board (from move) #\. history))
    (setf history (update-location board (to move) from-piece history))
    (setf (getf state :history) (cons history (getf state :history)))
    (setf (getf state :on-move) (opp-color (getf state :on-move)))
    ;;Needs to promote pawns here
    (when (eql (getf state :on-move) :white)
      (setf (getf state :turn) (+ 1 (getf state :turn))))
    state))

(defun undo-move (state)
  (let ((board (getf state :board))
        (history (car (getf state :history))))
    (when (not history) (return-from undo-move state))
    (setf (getf state :on-move) (opp-color (getf state :on-move)))
    (dolist (old-move history)
      (update-location-raw board (car old-move) (cdr old-move)))
    (when (eql (getf state :on-move) :black)
      (setf (getf state :turn) (- (getf state :turn) 1)))
    (setf (getf state :history) (cdr (getf state :history)))
    state))

(defun hash-state (state)
  "In a completely un-threadsafe way get the hash of the state without its history"
  (let ((temp-history (getf state :history)))
    (setf (getf state :history) nil)
    (let ((hash (sxhash state)))
      (setf (getf state :history) temp-history)
      hash)))

(defun game-status (state)
  ;; Stealing whoppers way of checking for kings here :)
  (let ((white-king nil)
        (black-king nil)
        (board (getf state :board)))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let ((piece (piece-at board (cons x y))))
                (cond
                  ((eql piece #\K) (setf white-king T))
                  ((eql piece #\k) (setf black-king T))))))

    (cond
      ((not white-king) :black)
      ((not black-king) :white)
      ((= 0 (length (possible-moves state))) :tie)
      (T :ongoing))))

;;Heuristic stuff

(defun piece-points (state)
  (let ((board (getf state :board))
        (score 0))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (setf score (+ score (piece-value (raw-piece-at board (cons x y)))))))
    (if (eql :white (getf state :on-move))
        score
        (- score))))

(defun score (state)
  (let* ((points (piece-points state))
         (status (game-status state)))
    (cond
      ((eql :draw status) 0)
      ((eql :ongoing status) points)
      (T (if (eql (getf state :on-move) status)
             10000
             10000)))))

(lazy-memoization game-status)
(lazy-memoization score)

(defparameter *node-counter 0)

;;(defun negamax-inner (state hueristic prune depth counter best))

;;(defun negamax (state hueristic prune depth))
