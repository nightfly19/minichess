(in-package :elo100)
(ql:quickload :lisp-unit)

(setf *print-failures* t)
(setf *print-errors* t)

(define-test make-state-test
  (assert-equalp (make-game-state) (make-game-state)))

(define-test negamax-sanity-test
  (with-state (make-game-state)
    (assert-equalp (nth-value 0 (negamax *game-state* #'score T 3))
                   '((1 . 5) 2 . 3)))
  (with-state (make-game-state)
    (assert-equalp (nth-value 0 (negamax *game-state* #'score T 4))
                   '((1 . 5) 0 . 3)))
  (with-state (make-game-state)
    (assert-equalp (nth-value 0 (negamax *game-state* #'score T 5))
                   '((2 . 4) 2 . 3)))
  (with-state (make-game-state)
    (assert-equalp (nth-value 0 (negamax *game-state* #'score T 6))
                   '((1 . 4) 1 . 3))))

(define-test negamax-test
  (with-state (make-game-state) #'score
              (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 1))
                             (nth-value 0 (negamax *game-state* #'score T 1))))
  (with-state (make-game-state) #'score
              (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 2))
                             (nth-value 0 (negamax *game-state* #'score T 2))))
  (with-state (make-game-state) #'score
              (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 3))
                             (nth-value 0 (negamax *game-state* #'score T 3))))
  (with-state (make-game-state) #'score
              (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 4))
                             (nth-value 0 (negamax *game-state* #'score T 4)))))

(defun run-unit-tests ()
  (let ((results (lisp-unit:run-tests :all :elo100)))
    (sb-ext:exit :code (+ (length (error-tests results))
                          (length (failed-tests results))))))

(defun move-list-contains (move-list move)
  (reduce (lambda (success pos-move)
            (or success
                (equalp move pos-move))) move-list :initial-value nil))
