(in-package :elo100)

;; (defparameter *ascii-table* (let ((table (make-array 255 :element-type 'character :initial-element #\Space)))
;;                                     (setf (aref table 97) #\a)
;;                                     (setf (aref table 98) #\b)
;;                                     (setf (aref table 99) #\c)
;;                                     (setf (aref table 100) #\d)
;;                                     (setf (aref table 101) #\e)
;;                                     table))

;; (defun print-state (state)
;;   (let ((board (game-state-board state)))
;;     (format T "~%")
;;     (format T "~A ~A~%" (if (eql :white (game-state-on-move state)) "W" "B") (game-state-turn state))
;;     (loop for row from 0 to 5 do
;;          (format T "~A~%" (aref board row)))))

(defun parse-coord (coord-string)
  (register-groups-bind (x y) ("^([a-e])(\\d)" coord-string)
    (cons (- (char-int (char x 0)) 97) (- 6 (parse-integer y)))))

(defun parse-move (move-string)
  (register-groups-bind (from to) ("^([a-e]\\d)-([a-e]\\d)" move-string)
    (cons (parse-coord from) (parse-coord to))))

(defun serialize-coord (coord)
  (format nil "~C~A" (aref *ascii-table* (+ 97 (car coord))) (+ 6 (- (cdr coord)))))

(defun serialize-move (move)
  (format nil "~A-~A" (serialize-coord (car move)) (serialize-coord (cdr move))))
