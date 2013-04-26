(in-package :elo100)

(defparameter *win-threshold* 10000)
(defparameter *node-counter* 0)
(defparameter *depth* 0)
(defparameter *max-depth* 0)

(defun piece-points (state)
  (let ((board state)
        (score 0))
    (declare (type fixnum score))
    (loop for y from 0 to 5 do
         (loop for x from 0 to 4 do
              (let ((piece (piece-at board x y)))
                (setf score (+ score
                               (if (= (piece-color piece) +white+)
                                   (+ (piece-value piece))
                                   (- (piece-value piece))))))))
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

(defun negamax-inner (prune depth ab)
  (let ((*depth* depth))
    (setf *node-counter* (+ 1 *node-counter*))
    (if (or (not (eql :ongoing (game-status-status *game-status*)))
            (>= depth *max-depth*))
        (cons (game-status-score *game-status*) (cdr ab))
        (destructuring-bind (best-move alpha beta) (cons nil ab)
          (dolist (possible-move (game-status-possible-moves *game-status*))
            ;; (sort (game-status-possible-moves *game-status*)
            ;;       (lambda (a b)
            ;;         (< (game-status-score
            ;;             (get-cached-status (apply-move *game-state* a) depth))
            ;;            (game-status-score
            ;;             (get-cached-status (apply-move *game-state* b) depth))))))
            (when (not (and prune (>= alpha beta)))
              (with-move possible-move
                (destructuring-bind (possible-alpha possible-beta)
                    (negate-negamax (negamax-inner prune (+ depth 1) (invert-negamax (list alpha beta))))
                  (when (> possible-alpha alpha)
                    (setf alpha possible-alpha)
                    (setf best-move possible-move))))))

          (values (list alpha beta) best-move)))))

(defun negamax (state heuristic prune max-depth)
  (with-state state
    (let* ((*node-counter* 0)
           (*heuristic* heuristic)
           (*game-status* (make-game-status))
           (*max-depth* max-depth)
           (*depth* 0)
           (move (nth-value 1 (negamax-inner prune 0 (list (- 0  *win-threshold* 1) (+ 1 *win-threshold*))))))
      (values move *node-counter*))))

(defun bot-move ()
  (let ((*move-application-cache-off* nil)
        (*transposition-table-off* nil))
    (negamax *game-state* #'score T 6)))
