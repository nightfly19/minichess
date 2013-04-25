(in-package :elo100)

(defun make-transposition-table ()
  (make-hash-table :size (expt 2 20) :test 'equal))

(defparameter *transposition-table* (make-transposition-table))
(defparameter *transposition-table-off* T)

(defun get-cached-status (state depth)
  (if *transposition-table-off*
      (with-state state (make-game-status))
      (let ((*game-state* state))
        (let* ((key state)
               (cached (gethash key *transposition-table* :not-found)))
          (if (and (not (eql cached :not-found))
                   (>= (cdr cached) depth))
              (car cached)
              (let ((new-value (make-game-status)))
                (setf (gethash key *transposition-table*) (cons new-value depth))
                new-value))))))
