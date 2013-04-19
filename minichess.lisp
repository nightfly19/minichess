
;;(ql:quickload :lisp-unit)

(declaim (optimize speed))

(defpackage :elo100
  (:use #:common-lisp))
;;        #:lisp-unit))

(in-package :elo100)

(defparameter *default-cache-size* (expt 2 24))

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


(defun new-board ()
  (copy-seq `#(,(copy-seq "kqbnr")
               ,(copy-seq "ppppp")
               ,(copy-seq ".....")
               ,(copy-seq ".....")
               ,(copy-seq "PPPPP")
               ,(copy-seq "RNBQK"))))

(defun new-state ()
  (copy-list `(:board ,(new-board)
                      :on-move :white
                      :turn 0)))

(defparameter *node-counter* 0)
(defparameter *win-threshold* 10000)
(defparameter *state* (new-state))
(defparameter *game-status* :ongoing)
(defparameter *score* 0)
(defparameter *possible-moves* nil)
(defparameter *state-history* nil)

(defmacro x (coord) `(car ,coord))

(defmacro y (coord) `(cdr ,coord))

(defmacro from (move) `(car ,move))

(defmacro to (move) `(cdr ,move))

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

(defun opp-color (color)
  (if (eql color :white) :black :white))

(defun move-scan (moves board color coord coord-d capture require-capture max-manhat)
  (let ((keep-searching T)
        (moves moves)
        (o-x (x coord))
        (o-y (y coord))
        (cur-x (x coord))
        (cur-y (y coord))
        (d-x (x coord-d))
        (d-y (y coord-d)))
    (declare (type fixnum cur-x d-x cur-y d-y o-x o-y max-manhat))
    (setf cur-x (+ cur-x d-x))
    (setf cur-y (+ cur-y d-y))
    (loop while (and keep-searching
                     (d-in-bounds-p cur-x cur-y)
                     (<= (d-manhat o-x o-y cur-x cur-y) max-manhat)) do
         (let ((piece (d-piece-at board cur-x cur-y)))
           (if piece
               (progn
                 (cond
                   ((not capture)
                    (setf keep-searching nil))
                   ((eql color (piece-color piece))
                    (setf keep-searching nil))
                   (T (setf keep-searching nil)
                      (setf moves (cons (cons coord (cons cur-x cur-y)) moves)))))
               (when (not require-capture)
                 (setf moves (cons (cons coord (cons cur-x cur-y)) moves)))))
         (setf cur-x (+ cur-x d-x))
         (setf cur-y (+ cur-y d-y)))
    moves))

(defun mover (action directions moves)
  (reduce action directions :initial-value moves))

(defgeneric inner-move-list (board color coord moves piece-class))
(defmethod inner-move-list (board color coord moves piece-class) moves)

(defmethod inner-move-list (board color coord moves (piece-class (eql #\N)))
  (mover (lambda (moves direction)
           (move-scan moves board color coord direction T nil 3))
         '((-1 . 2) (1 . 2)   (2 . 1)   (2 . -1)
           (1 . -2) (-1 . -2) (-2 . -1) (-2 . 1)) moves))

(defmethod inner-move-list (board color coord moves (piece-class (eql #\R)))
  (mover (lambda (moves direction)
           (move-scan moves board color coord direction T nil 99))
         '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))

(defmethod inner-move-list (board color coord moves (piece-class (eql #\Q)))
  (mover (lambda (moves direction)
           (move-scan moves board color coord direction T nil 99))
         '((-1 . 1) (0 . 1) (1 . 1)
           (-1 . 0)         (1 . 0)
           (-1 . -1)(0 . -1)(1 . -1)) moves))

(defmethod inner-move-list (board color coord moves (piece-class (eql #\K)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board color coord direction T nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board color coord direction T nil 2))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    moves))

(defmethod inner-move-list (board color coord moves (piece-class (eql #\B)))
  (let ((moves moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board color coord direction T nil 99))
                       '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)) moves))
    (setf moves (mover (lambda (moves direction)
                         (move-scan moves board color coord direction nil nil 1))
                       '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) moves))
    moves))

(defmethod inner-move-list (board color coord moves (piece-class (eql #\P)))
  (let ((color (color-at board coord)))
    (mover (lambda (moves direction)
             (move-scan moves board color coord direction nil nil 1))
           (if (eql color :white) '((0 . -1)) '((0 . 1)))
           (mover (lambda (moves direction)
                    (move-scan moves board color coord direction T T 2))
                  (if (eql color :white)
                      '((1 . -1) (-1 . -1))
                      '((1 . 1) (-1 . 1))) moves))))

(defun move-list (board coord moves)
  (inner-move-list board (color-at board coord) coord moves (piece-class (piece-at board coord))))

(defun possible-moves ()
  (let ((board (getf *state* :board))
        (color (getf *state* :on-move))
        (moves ()))
    (declare (type (simple-vector 6) board))
    (loop for y from 0 to 5 do
         (let ((row (aref board y)))
           (loop for x from 0 to 4 do
                (let* ((piece (char row x))
                       (spot-color (piece-color piece)))
                  (when (eql color spot-color)
                    (setf moves (move-list board (cons x y) moves)))))))
    moves))

(defmacro update-location-raw (board coord piece)
  `(setf (char (aref ,board (y ,coord)) (x ,coord)) ,piece))

(defun update-location (board coord piece history)
  (let ((old-piece (raw-piece-at board coord)))
    (setf (char (aref board (y coord)) (x coord)) piece)
    (cons (cons coord old-piece) history)))

(defun update-row (row x y piece history)
  (let ((old-piece (char row x)))
    (setf (char row x) piece)
    (cons (cons (cons x y) old-piece) history)))

(defun promote-pawns (board history)
  (declare (type (simple-vector 6) board))
  (let ((history history))
    (dolist (y '(0 5))
      (let ((row (aref board y)))
        (loop for x from 0 to 4 do
             (let ((piece (char row x)))
               (cond
                 ((eql piece #\p) (setf history (update-row row x y #\q history)))
                 ((eql piece #\P) (setf history (update-row row x y #\P history))))))))
    history))

(defun apply-move (move)
  (let* ((state *state*)
         (board (getf state :board))
         (history nil)
         (from-piece (raw-piece-at board (from move))))
    (setf history (update-location board (from move) #\. history))
    (setf history (update-location board (to move) from-piece history))
    (setf history (promote-pawns board history))
    (setf *state-history* (cons history *state-history*))
    (setf (getf state :on-move) (opp-color (getf state :on-move)))
    (when (eql (getf state :on-move) :white)
      (setf (getf state :turn) (+ 1 (getf state :turn))))
    state))

(defun undo-move ()
  (let* ((state *state*)
         (board (getf state :board))
         (history (car *state-history*)))
    (when history
      (setf (getf state :on-move) (opp-color (getf state :on-move)))
      (dolist (old-move history)
        (update-location-raw board (car old-move) (cdr old-move)))
      (when (eql (getf state :on-move) :black)
        (setf (getf state :turn) (- (getf state :turn) 1)))
      (setf *state-history* (cdr *state-history*))
      state)))

(defun game-status ()
  (declare (type cons *possible-moves*))
  ;; Stealing whoppers way of checking for kings here :)
  (let ((white-king nil)
        (black-king nil)
        (board (getf *state* :board)))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let ((piece (piece-at board (cons x y))))
                (cond
                  ((eql piece #\K) (setf white-king T))
                  ((eql piece #\k) (setf black-king T))))))
    (cond
      ((not white-king) :black)
      ((not black-king) :white)
      ((= 0 (length *possible-moves*)) :tie)
      (T :ongoing))))

(defun piece-points (state)
  (let ((board (getf state :board))
        (score 0))
    (declare (type fixnum score))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (setf score (+ score (piece-value (raw-piece-at board (cons x y)))))))
    (if (eql :white (getf state :on-move))
        score
        (- score))))

(defun score ()
  (let* ((points (piece-points *state*))
         (status *game-status*))
    (cond
      ((eql :draw status) 0)
      ((eql :ongoing status) points)
      (T (if (eql (getf *state* :on-move) status)
             10000
             10000)))))

(defun negate-negamax (ab)
  (list (* -1 (car ab))
        (* -1 (cadr ab))))

(defun invert-negamax (ab)
  (list (* -1 (cadr ab))
        (* -1 (car ab))))

(defun print-state (state)
  (let ((board (getf state :board)))
    (loop for row from 0 to 5 do
         (format T "~A~%" (aref board row)))))

(defmacro with-move (move heuristic &body forms)
  (let ((result (gensym "result")))
    `(progn
       (when ,move
         (apply-move ,move))
       (let* ((*possible-moves* (possible-moves))
              (*game-status* (game-status))
              (*score* (funcall ,heuristic))
              (,result (multiple-value-list (progn ,@forms))))
         (undo-move)
         (values-list ,result)))))

(defmacro with-state (state heuristic &body forms)
  `(let ((*state* ,state)
         (*state-history* nil))
     (with-move nil ,heuristic ,@forms)))

(defun negamax-inner (heuristic prune depth ab)
  (setf *node-counter* (+ 1 *node-counter*))
  (if (or (not (eql :ongoing *game-status*))
          (<= depth 0))
      (cons *score* (cdr ab))
      (destructuring-bind (best-move alpha beta) (cons nil ab)
        (dolist (possible-move *possible-moves*)
          (when (not (and prune (> alpha beta)))
            (with-move possible-move heuristic
              (destructuring-bind (possible-alpha possible-beta)
                  (negate-negamax (negamax-inner
                                   heuristic prune (- depth 1) (invert-negamax (list alpha beta))))
                (when (> possible-alpha alpha)
                  (setf alpha possible-alpha)
                  (setf best-move possible-move))
                (when (> possible-beta beta)
                  (setf beta possible-beta))))))
        (values (list alpha beta) best-move))))

(defun negamax (state heuristic prune depth)
  (with-state state heuristic
    (let* ((*node-counter* 0)
           (move (nth-value 1 (negamax-inner heuristic prune depth (list (- 0  *win-threshold* 1) (+ 1 *win-threshold*))))))
      (values move *node-counter*))))

