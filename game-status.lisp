(in-package :elo100)

(defparameter *heuristic* (constantly 0))
(defparameter *game-status* nil)

(defstruct (game-status
             (:constructor make-game-status
                           (&key (possible-moves (possible-moves *game-state*))
                                 (status (game-state-status *game-state* possible-moves))
                                 (score (funcall *heuristic* *game-state* status)))))
  (possible-moves (possible-moves *game-state* T))
  (score (funcall *heuristic* *game-state*))
  (status))

(defmacro with-move-lazy (move lazy &body forms)
  (let ((result (gensym "result")))
    `(progn
       (game-state-apply-move *game-state* ,move)
       (let* ((*game-status* ,(if (not lazy) '(get-cached-status *game-state* *depth*) nil))
              (,result (multiple-value-list (progn ,@forms))))
         (game-state-undo-move *game-state*)
         (values-list ,result)))))

(defmacro with-move (move &body forms)
  `(with-move-lazy ,move nil ,@forms))
