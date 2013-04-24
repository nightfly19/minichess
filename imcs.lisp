(in-package :elo100)

(defparameter *username* "elo100")
(defparameter *password* "minichess")

(defparameter *imcs-stream* T)
(defparameter *imcs-player* (constantly nil))
(defparameter *imcs-color* :white)

(defun format-color-string (color)
  (cond ((eql color :white) "W")
        ((eql color :black) "B")
        (T "")))

(defun get-imcs-server-stream ()
  (let* ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
         (stream (socket-make-stream socket :input T :output T :buffering :line)))
    (socket-connect socket #(131 252 214 11) 3589)
    (values stream socket)))

(defun print-line (stream line)
  (format stream "~A~C" line #\Newline))

(defun imcs-login (stream)
  ;; Assume username was accepted
  (print-line T (read-line stream)))

(defgeneric imcs-code (number first-line &optional callback))

(defun imcs-response-dispatcher (&optional callback)
  (register-groups-bind (code first-line) ("^(\\d+)\\s+(.*)" (read-line *imcs-stream*))
    (imcs-code (parse-integer code) first-line callback)))

(defmethod imcs-code ((number T) first-line &optional callback)
  (declare (ignore callback))
  (format T "Unhandled: ~A ~A~%" number first-line))

(defmethod imcs-code ((number (eql 100)) first-line &optional callback)
  (print-line T first-line)
  (print-line *imcs-stream* (format nil "me ~A ~A~%" *username* *password*))
  (let ((response (imcs-response-dispatcher)))
    (when (eql response 201)
      (if callback (funcall callback number first-line) T))))


(defmethod imcs-code ((number (eql 103)) line &optional callback)
  (print-line T line)
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 201)) first-line &optional callback)
  (print-line T "Got hello")
  (print callback)
  (if callback (funcall callback number first-line) number))

(defmethod imcs-code ((number (eql 105)) line &optional callback)
  "game starts (on-move)"
  (setf *imcs-color* :white)
  ;;read state
  ;;read time-remaining ? 05:00.000 03:59.384
  ;;game play here

  ;;read mode ! e5-e4
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 106)) line &optional callback)
  "game starts (not on-move)"
  (setf *imcs-color* :black)
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 230)) line &optional callback)
  "draw"
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 231)) line &optional callback)
  "White wins"
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 232)) line &optional callback)
  "Black wins"
  (if callback (funcall callback number line) number))

(defmethod imcs-code ((number (eql 401)) line &optional callback)
  "Unknown password"
  (if callback (funcall callback number line) number))

(defgeneric imcs-play-message (token message))

(defun imcs-session (callback)
  (let ((*imcs-stream* (get-imcs-server-stream))
        (*imcs-color* :white))
    (imcs-response-dispatcher callback)))

(defun offer-game (&optional color (time "") (opp-time time))
  (imcs-session (lambda (&rest rest)
                  (declare (ignore rest))
                  (format *imcs-stream* "offer ~A ~A ~A~%" (format-color-string color) time opp-time)
                  (imcs-response-dispatcher
                   (lambda (&rest rest) (imcs-response-dispatcher))))))

(defun accept-game (game-id &optional (color ""))
  (imcs-session (lambda (&rest rest)
                  (declare (ignore rest))
                  (format *imcs-stream* "accept ~A ~A~%" game-id (format-color-string color))
                  (imcs-response-dispatcher))))
