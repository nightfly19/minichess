(in-package :elo100)

(defparameter *imcs-username* "elo100")
(defparameter *imcs-password* "minichess")
(defparameter *imcs-stream* T)
(defparameter *imcs-request-string* "")
(defparameter *imcs-player* #'random-move)
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

(defun discard-state-on-stream (stream)
  (dotimes (i 9)
    (format T "~A~%" (read-line stream)))
  nil)

(defun imcs-login ()
  (print-line *imcs-stream* (format nil "me ~A ~A" *imcs-username* *imcs-password*)))

(defun imcs-register ()
  (print-line *imcs-stream* (format nil "register ~A ~A" *imcs-username* *imcs-password*)))

(defun imcs-send-request-string ()
  (print-line *imcs-stream* *imcs-request-string*))

(defun imcs-message-dispatcher ()
  (register-groups-bind (token first-line) ("^(\\S+)\\s+(.+)\\r$" (read-line *imcs-stream*))
    (imcs-message (intern token "KEYWORD") first-line)))

(defmethod imcs-message ((token T) message)
  (format T "Unhandled: ~A ~A~%" token message)
  (values token message))

(defmethod imcs-message ((token (eql :|100|)) message)
  (print-line T message)
  (imcs-login)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|400|)) message)
  (print-line T "Unregistered nick")
  (imcs-register)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|202|)) message)
  "Successfully registered"
  (print-line T message)
  (imcs-send-request-string)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|201|)) message)
  (print-line T message)
  (imcs-send-request-string)
  (imcs-message-dispatcher))
;;  (multiple-value-bind (code message) (imcs-message-dispatcher)
;;    (cond (eql :l

(defmethod imcs-message ((token (eql :|103|)) line)
  "Game waiting for acceptance"
  (print-line T line)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|105|)) line)
  "Game starts (on-move)"
  (print-line T line)
  (setf *imcs-color* :white)
  (discard-state-on-stream *imcs-stream*)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|106|)) line)
  "Game starts"
  (print-line T line)
  ;;need to parse play time here
  (setf *imcs-color* :black)
  (imcs-message-dispatcher))

(defmethod imcs-message ((token (eql :|!|)) line)
  (format T "Got   : ~A~%" line)
  (print-line T line)
  (discard-state-on-stream *imcs-stream*)
  (with-move (parse-move line)
    (imcs-message-dispatcher)))

(defmethod imcs-message ((token (eql :|?|)) line)
  (print-line T line)
  (let ((move (funcall *imcs-player*)))
    (print-line *imcs-stream* (serialize-move move))
    (format T "Moving: ~A~%" (serialize-move move))
    (with-move move
      (imcs-message-dispatcher))))

(defmethod imcs-message ((tolen (eql :|-|)) line)
  (print-line T line))

(defmethod imcs-message ((token (eql :|X|)) line)
  (print-line T "Opponant disconnected"))

(defmethod imcs-message ((token (eql :|421|)) line)
  (print-line T "Offers canceled"))

(defmethod imcs-message ((token (eql :|231|)) line)
  (print-line T "White wins!"))

(defmethod imcs-message ((token (eql :|232|)) line)
  (print-line T "black wins!"))

(defmethod imcs-message ((token (eql :|=|)) line)
  (print-line T line))

(defun imcs-session ()
  (let ((*imcs-stream* (get-imcs-server-stream))
        (*imcs-color* :white))
    (imcs-message-dispatcher)))

(defun offer-game (&optional color (time "") (opp-time time))
  (with-state (make-state)
    (let* ((*imcs-request-string*
            (format nil "offer ~A ~A ~A" (format-color-string color) time opp-time))
           (*imcs-player* #'bot-move))
      (imcs-session))))

(defun accept-game (game-id &optional (color ""))
  (with-state (make-state)
    (let* ((*imcs-request-string*
            (format nil "accept ~A ~A" game-id (format-color-string color)))
           (*imcs-player* #'bot-move))
      (imcs-session))))
