(in-package :elo100)

(defun print-state (state)
  (let ((board (getf state :board)))
    (format T "~%")
    (format T "~A ~A~%" (if (eql :white (getf state :on-move)) "W" "B") (getf state :turn))
    (loop for row from 0 to 5 do
         (format T "~A~%" (aref board row)))))

