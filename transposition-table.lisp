(in-package :elo100)

(defparameter *score-cache-size* 2013377)
;;(defparameter *score-cache-size* 1000003)
;;(defparameter *score-cache-size* 200003)
;;(defparameter *score-cache-size* 23)
(defparameter *score-cache-off* nil)
(defparameter *score-cache* (make-array *score-cache-size* :element-type 'cons :initial-element nil))
(defparameter *score-cache-hit* 0)
(defparameter *score-cache-early-miss* 0)
(defparameter *score-cache-late-miss* 0)

(defun score-cache-in-use ()
  (reduce (lambda (score element)
            (if element (+ 1 score) score)) *score-cache* :initial-value 0))

(defun score-cache-key (state)
  (mod (game-state-hash state) *score-cache-size*))

(defun get-cached-score (state)
  (if *score-cache-off*
      0
      (let* ((key (score-cache-key state))
             (raw-cached (aref *score-cache* key)))
        (if raw-cached
            (destructuring-bind (c-score c-board-a c-board-b depth) raw-cached
              (if t;;(and (= (game-state-board-a state) c-board-a)
                  ;;     (= (game-state-board-b state) c-board-b))
                  (progn
                    (incf *score-cache-hit*)
                    c-score)
                  (progn
                    (incf *score-cache-late-miss*)
                    0)))
            (progn
              (incf *score-cache-early-miss*)
                    0)))))

(defun cache-score (state depth score)
  (let* ((key (score-cache-key state))
         (cached (aref *score-cache* key)))
    (when (and (not *score-cache-off*)
               (or (eql cached nil)
                   (> depth (nth 3 cached))))
      (setf (aref *score-cache* key)
            (list score
                  (game-state-board-a state)
                  (game-state-board-b state)
                  depth)))))

(defparameter *status-cache-size* (expt 2 16))
(defparameter *status-cache-off* T)
(defparameter *status-cache* (make-array *status-cache-size* :element-type 'cons :initial-element nil))

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
