(in-package :elo100)

(defun make-status-cache ()
  (make-hash-table :size (expt 2 20) :test 'equal))

(defparameter *status-cache* (make-status-cache))
(defparameter *status-cache-off* nil)

(defun get-cached-status (state depth)
  (if *status-cache-off*
      (with-state state (make-game-status))
      (let ((*game-state* state))
        (let* ((key state)
               (cached (gethash key *status-cache* :not-found)))
          (if (and (not (eql cached :not-found))
                   (>= (cdr cached) depth))
              (car cached)
              (let ((new-value (make-game-status)))
                (setf (gethash key *status-cache*) (cons new-value depth))
                new-value))))))

(defparameter *weak-status-cache-size* (expt 2 20))
(defparameter *weak-status-cache* (make-array *weak-status-cache-size* :element-type 'cons :initial-element nil))
(defparameter *weak-status-cache-off* nil)

(defun get-weak-cached-status (state depth)
  (if *weak-status-cache-off*
      (with-state state (make-game-status))
      (let* ((*game-state* state)
             (key (mod (sxhash state) *weak-status-cache-size*))
             (cached (aref *weak-status-cache* key)))
        (if (and cached
                 (>= (cdr cached) depth))
            (car cached)
            (let ((new-value (make-game-status)))
              (setf (aref *weak-status-cache* key) (cons new-value depth))
              new-value)))))
