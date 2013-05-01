(in-package :elo100)

(defparameter *win-threshold* 10000)
(defparameter *node-counter* 0)
(defparameter *depth* 0)
(defparameter *max-depth* 0)
(defparameter *time-cutoff* 0)
(defparameter *bot-move-time* 5)

(define-condition out-of-time (error)
  ())

(defmacro within-time-p ()
  `(or (= 0 *time-cutoff*)
       (< (get-internal-run-time) *time-cutoff*)))

(defun check-within-time ()
  (when (not (within-time-p))
    (error 'out-of-time)))

(defun piece-points (state)
  (let* ((board-a (game-state-board-a state))
         (board-b (game-state-board-b state))
         (score 0))
    (declare (type fixnum score))
    (loop for location from 0 to (* 14 +piece-size+) by +piece-size+ do
         (let ((piece-a (ldb (byte +piece-size+ location) board-a))
               (piece-b (ldb (byte +piece-size+ location) board-b)))
           (setf score (+ score
                          (if (= (piece-color piece-a) +white+)
                              (+ (piece-value piece-a))
                              (- (piece-value piece-a)))))
           (setf score (+ score
                          (if (= (piece-color piece-b) +white+)
                              (+ (piece-value piece-b))
                              (- (piece-value piece-b)))))))
    (if (eql +white+ (game-state-on-move state))
        score
        (- score))))

(defun score (state status)
  (let ((points (piece-points state)))
    (cond
      ((eql :draw status) 0)
      ((eql :ongoing status) points)
      (T (if (eql (game-state-on-move state) status)
             10000
             -10000)))))

(defun negate-negamax (ab)
  (list (* -1 (car ab))
        (* -1 (cadr ab))))

(defun invert-negamax (ab)
  (list (* -1 (cadr ab))
        (* -1 (car ab))))

(labels ((negamax-inner (prune depth ab)
           (check-within-time)
           (let ((*depth* depth))
             (setf *node-counter* (+ 1 *node-counter*))
             (if (or (not (eql :ongoing (game-status-status *game-status*)))
                     (>= depth *max-depth*))
                 (cons (game-status-score *game-status*) (cdr ab))
                 (destructuring-bind (best-move alpha beta) (cons nil ab)
                   (dolist (possible-move
                             (sort (possible-moves *game-state*)
                                   (lambda (a b)
                                     (> (with-move-lazy a t (get-cached-score *game-state*))
                                        (with-move-lazy b t (get-cached-score *game-state*))))))
                     (when (not (and prune (>= alpha beta)))
                       (destructuring-bind (possible-alpha possible-beta)
                           (with-move possible-move
                             (negate-negamax (negamax-inner prune (+ depth 1) (invert-negamax (list alpha beta)))))
                         (when (> possible-alpha alpha)
                           (cache-score *game-state* depth possible-alpha)
                           (setf alpha possible-alpha)
                           (setf best-move possible-move)))))

                   (values (list alpha beta) best-move))))))

  (defun negamax (state heuristic prune max-depth)
    (with-state state
      (let* ((*node-counter* 0)
             (*heuristic* heuristic)
             (*game-status* (make-game-status))
             (*max-depth* max-depth)
             (*depth* 0)
             (move (nth-value 1 (negamax-inner prune 0 (list (- 0  *win-threshold* 1) (+ 1 *win-threshold*))))))
        (values move *node-counter*)))))

(defun iterative-deepening (state heuristic prune max-depth time-for-move)
  (let ((*time-cutoff* (+ (* internal-time-units-per-second time-for-move)
                          (get-internal-run-time)))
        (best-move (list nil 0)))
    (handler-case
        (loop for d from 1 to max-depth do
             (let ((new-move (negamax state heuristic prune d)))
               (when (not (eql :out-of-time new-move))
                 (setf best-move (list new-move d)))))
      (out-of-time () (progn
                        best-move)))
    (values-list best-move)))

(defun bot-move ()
  (iterative-deepening *game-state* #'score T 12 *bot-move-time*))
