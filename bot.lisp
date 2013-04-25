(in-package :elo100)

(defparameter *win-threshold* 10000)
(defparameter *node-counter* 0)
(defparameter *depth* 0)
(defparameter *max-depth* 0)

(defun piece-points (state)
  (let ((board (game-state-board state))
        (score 0))
    (declare (type fixnum score))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (setf score (+ score (piece-value (raw-piece-at board (cons x y)))))))
    (if (eql :white (game-state-on-move state))
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

(defmacro with-move (move &body forms)
  (let ((result (gensym "result")))
    `(progn
       (when ,move
         (apply-move ,move))
       (let* ((*game-status* (get-cached-status *game-state* *depth*))
              (,result (multiple-value-list (progn ,@forms))))
         (when ,move
           (undo-move))
         (values-list ,result)))))

(defmacro with-state (state &body forms)
  `(let ((*game-state* ,state))
     (with-move nil ,@forms)))

(defun negamax-inner (prune depth ab)
  ;;(print "mew")
  ;;(print (game-status-score *game-status*))
  (let ((*depth* depth))
    (setf *node-counter* (+ 1 *node-counter*))
    (if (or (not (eql :ongoing (game-status-status *game-status*)))
            (>= depth *max-depth*))
        (cons (game-status-score *game-status*) (cdr ab))
        (destructuring-bind (best-move alpha beta) (cons nil ab)
          (dolist (possible-move (sort (game-status-possible-moves *game-status*)
                                       (lambda (a b)
                                         (<
                                          (with-move a
                                            (game-status-score
                                             (get-cached-status *game-state* depth)))
                                          (with-move b
                                            (game-status-score
                                             (get-cached-status *game-state* depth)))))))
            (when (not (and prune (> alpha beta)))
              (with-move possible-move
                (destructuring-bind (possible-alpha possible-beta)
                    (negate-negamax (negamax-inner prune (+ depth 1) (invert-negamax (list alpha beta))))
                  (when (> possible-alpha alpha)
                    (setf alpha possible-alpha)
                    (setf best-move possible-move))
                  (when (> possible-beta beta)
                    (setf beta possible-beta))))))
          (values (list alpha beta) best-move)))))

(defun negamax (state heuristic prune max-depth)
  (with-state state heuristic
              (let* ((*node-counter* 0)
                     (*heuristic* heuristic)
                     (*max-depth* max-depth)
                     (*depth* 0)
                     (move (nth-value 1 (negamax-inner prune 0 (list (- 0  *win-threshold* 1) (+ 1 *win-threshold*))))))
                (values move *node-counter*))))


(defun bot-move ()
  (negamax *game-state* #'score T 5))
