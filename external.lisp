(in-package :elo100)

(defun print-state (state)
  (let ((board (getf state :board)))
    (loop for row from 0 to 5 do
         (format T "~A~%" (aref board row)))))

