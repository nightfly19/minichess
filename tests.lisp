(in-package :elo100)
(ql:quickload :lisp-unit)

(setf *print-failures* t)
(setf *print-errors* t)

(define-test make-state-test
  (assert-equalp (make-state) (make-state)))

(define-test apply-move
  (with-state (make-state) #'score
    (apply-move  '((1 . 1) 3 . 3))
    (assert-equalp #("kqbnr" "p.ppp" "....." "...p." "PPPPP" "RNBQK")
                   (getf *state* :board)))
  (with-state (make-state) #'score
    (apply-move  '((2 . 1) 4 . 2))
    (assert-equalp #("kqbnr" "pp.pp" "....p" "....." "PPPPP" "RNBQK")
                   (getf *state* :board)))
  (with-state (make-state) #'score
    (apply-move  '((4 . 5) 1 . 1))
    (assert-equalp #("kqbnr" "pKppp" "....." "....." "PPPPP" "RNBQ.")
                   (getf *state* :board))))

(define-test pawn-test
  (with-state (make-state) #'score
    (apply-move  '((1 . 1) 3 . 2))
    (assert-equalp '(((3 . 2) 3 . 3))
                   (move-list (getf *state* :board) '(3 . 2) nil)))
  (with-state (make-state) #'score
    (apply-move  '((1 . 1) 3 . 3))
    (assert-equalp '(((3 . 3) 2 . 4) ((3 . 3) 4 . 4))
                   (move-list (getf *state* :board) '(3 . 3) nil))))

(define-test negamax-test
  (with-state (make-state) #'score
    (assert-equalp (nth-value 0 (negamax *state* #'score nil 1))
                   (nth-value 0 (negamax *state* #'score T 1))))
  (with-state (make-state) #'score
    (assert-equalp (nth-value 0 (negamax *state* #'score nil 2))
                   (nth-value 0 (negamax *state* #'score T 2))))
  (with-state (make-state) #'score
    (assert-equalp (nth-value 0 (negamax *state* #'score nil 3))
                   (nth-value 0 (negamax *state* #'score T 3))))
  (with-state (make-state) #'score
    (assert-equalp (nth-value 0 (negamax *state* #'score nil 4))
                   (nth-value 0 (negamax *state* #'score T 4)))))


(defun run-unit-tests ()
  (let ((results (lisp-unit:run-tests :all :elo100)))
    (sb-ext:quit :unix-status (+ (length (error-tests results))
                                 (length (failed-tests results))))))

(defun move-list-contains (move-list move)
  (reduce (lambda (success pos-move)
            (or success
                (equalp move pos-move))) move-list :initial-value nil))

;;   (setf  (make-state))
;;   (setf *state-history* nil)
;;   (apply-move  '((1 . 1) 3 . 3))
;;   (print-state )
;;   (print (getf  :on-move))
;;   (print (possible-moves ))
;;   (assert-true (move-list-contains (possible-moves ) '((1 . 0) 1 . 4)))
;;   (assert-true (move-list-contains (possible-moves ) '((3 . 3) 4 . 4)))
;;   (assert-true (move-list-contains (possible-moves ) '((3 . 3) 2 . 4)))
;;   (dolist (pos-move (possible-moves ))
;;     (print pos-move)
;;     ;;(apply-move  pos-move)
;;     (format T "~%")
;;     (print-state )
;;     (print (score ))
;;     (undo-move )))
;; (setf  (make-state))
;; (setf *state-history* nil)
;; (assert-equalp (negamax  #'score nil 5)
;;                 (negamax  #'score T 5)))
