(in-package :elo100)

(defparameter *score-cache-size* (expt 2 16))
(defparameter *score-cache-off* T)
(defparameter *score-cache* (make-array *move-application-cache-size* :element-type 'cons :initial-element nil))

(defun get-cached-score (state depth &optional move)
  (if *score-cache-off*
      (values 0 0)
      (let ((key (mod (sxhash (list state move depth)) *score-cache-size*)))
        (destructuring-bind (c-state c-move c-depth c-score) (aref *score-cache* key)
          (if (and (not (eql c-state nil))
                   (= state c-state)
                   (= move c-move)
                   (>= c-depth depth))
              (values c-score c-depth)
              (values 0 0))))))

(defun cache-score (state depth score &optional move)
  (when (> depth (nth-value 1 (get-cached-score state depth move)))
    (setf (aref *score-cache* (mod (sxhash (list state move depth)) *score-cache-size*)) score)))

(defparameter *status-cache-size* (expt 2 16))
(defparameter *status-cache-off* T)
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
(defparameter *weak-status-cache-off* T)
(defparameter *weak-status-cache* (make-array *weak-status-cache-size* :element-type 'cons :initial-element nil))

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
