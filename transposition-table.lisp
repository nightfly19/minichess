(in-package :elo100)

(defparameter *status-cache-size* (expt 2 16))
(defparameter *status-cache-off* nil)
(defparameter *status-cache* (make-array *move-application-cache-size* :element-type 'cons :initial-element nil))

(defun get-cached-status (state depth)
  (if *status-cache-off*
      (with-state state (make-game-status))
      (let* ((key (mod (sxhash (cons state depth)) *status-cache-size*))
             (cached (aref *status-cache* key)))
        (if (and cached
                 (equal state (cadr cached))
                 (equal depth (cddr cached)))
            (car cached)
            (let ((new-value (with-state state (make-game-status))))
              (setf (aref *status-cache* key) (cons new-value (cons state depth)))
              new-value)))))

(defparameter *weak-status-cache-size* (expt 2 16))
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
