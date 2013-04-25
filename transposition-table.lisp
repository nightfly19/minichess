(in-package :elo100)

(defun make-transposition-table ()
  (make-hash-table :size (expt 2 20)))

(defparameter *transposition-table* (make-transposition-table))

(defun get-score (state depth)
  (let* ((key (state-to-integer *state*))
         (cached (gethash *transposition-table* key :not-found)))
    (if (and (not (eql cached :not-found))
             (>= (cdr cached) depth))
        (car cached)
        (let ((new-value (funcall *heuristic* state)))
          (setf (gethash *transposition-table* key) (cons new-value depth))
          new-value))))
