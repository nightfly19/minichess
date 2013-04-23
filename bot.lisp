(in-package :elo100)

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
       (let* ((*possible-moves* (possible-moves))
              (*game-status* (game-status))
              (*score* (funcall *heuristic*))
              (,result (multiple-value-list (progn ,@forms))))
         (undo-move)
         (values-list ,result)))))

(defmacro with-state (state &body forms)
  `(let ((*state* ,state)
         (*state-history* nil))
     (with-move nil ,@forms)))

(defun negamax-inner (prune depth ab)
  (setf *node-counter* (+ 1 *node-counter*))
  (if (or (not (eql :ongoing *game-status*))
          (<= depth 0))
      (cons *score* (cdr ab))
      (destructuring-bind (best-move alpha beta) (cons nil ab)
        (dolist (possible-move *possible-moves*)
          (when (not (and prune (> alpha beta)))
            (with-move possible-move
              (destructuring-bind (possible-alpha possible-beta)
                  (negate-negamax (negamax-inner prune (- depth 1) (invert-negamax (list alpha beta))))
                (when (> possible-alpha alpha)
                  (setf alpha possible-alpha)
                  (setf best-move possible-move))
                (when (> possible-beta beta)
                  (setf beta possible-beta))))))
        (values (list alpha beta) best-move))))

(defun negamax (state heuristic prune depth)
  (with-state state heuristic
    (let* ((*node-counter* 0)
           (*heuristic* heuristic)
           (move (nth-value 1 (negamax-inner prune depth (list (- 0  *win-threshold* 1) (+ 1 *win-threshold*))))))
      (values move *node-counter*))))


(defun bot-move ()
    (negamax *state* #'score T 4))
