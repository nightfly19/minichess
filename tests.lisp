(in-package :elo100)
(ql:quickload :lisp-unit)

(setf *print-failures* t)
(setf *print-errors* t)

(define-test game-state-initially-sane
  (assert-equalp (make-initial-game-state) (make-initial-game-state)))

(define-test game-state-apply-undo-functional
  (let ((state-a (make-initial-game-state))
        (state-b (make-initial-game-state)))
    (game-state-apply-move state-a '((1 . 5) 3 . 3))
    (assert-false (equalp state-a state-b))
    (game-state-undo-move state-a)
    (assert-true (equalp state-a state-b))))

(define-test possible-moves-sane
  (assert-equalp '(((1 . 5) 0 . 3) ((1 . 5) 2 . 3) ((4 . 4) 4 . 3) ((3 . 4) 3 . 3)
                   ((2 . 4) 2 . 3) ((1 . 4) 1 . 3) ((0 . 4) 0 . 3))
                 (possible-moves (make-initial-game-state))))

;; (define-test make-state-test
;;   (assert-equalp (make-game-state) (make-game-state)))

;; (define-test piece-at-test
;;   (with-state (make-game-state)
;;     (let* ((half-board-size (* 15 +piece-size+))
;;            (board-a (ldb (byte half-board-size +board-offset+) *game-state*))
;;            (board-b (ldb (byte half-board-size (+ +board-offset+
;;                                                   half-board-size)) *game-state*)))
;;       (assert-equalp (piece-at *game-state* 1 1)
;;                      (fast-piece-at 1 1 board-a board-b))
;;       (assert-equalp (piece-at *game-state* 0 4)
;;                      (fast-piece-at 0 4 board-a board-b))
;;       (assert-equalp (piece-at *game-state* 0 4)
;;                      (fast-piece-at 0 4 board-a board-b))
;;       (assert-equalp (piece-at *game-state* 4 5)
;;                      (fast-piece-at 4 5 board-a board-b)))))

;; (define-test negamax-sanity-test
;;   (with-state (make-game-state)
;;     (assert-equalp '((1 . 5) 2 . 3)
;;                    (nth-value 0 (negamax *game-state* #'score T 3))))
;;   (with-state (make-game-state)
;;     (assert-equalp '((1 . 5) 0 . 3)
;;                    (nth-value 0 (negamax *game-state* #'score T 4))))
;;   (with-state (make-game-state)
;;     (assert-equalp '((2 . 4) 2 . 3)
;;      (nth-value 0 (negamax *game-state* #'score T 5))))
;;   (with-state (make-game-state)
;;     (assert-equalp '((1 . 4) 1 . 3)
;;      (nth-value 0 (negamax *game-state* #'score T 6)))))

;; (define-test negamax-test
;;   (with-state (make-game-state) #'score
;;               (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 1))
;;                              (nth-value 0 (negamax *game-state* #'score T 1))))
;;   (with-state (make-game-state) #'score
;;               (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 2))
;;                              (nth-value 0 (negamax *game-state* #'score T 2))))
;;   (with-state (make-game-state) #'score
;;               (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 3))
;;                              (nth-value 0 (negamax *game-state* #'score T 3))))
;;   (with-state (make-game-state) #'score
;;               (assert-equalp (nth-value 0 (negamax *game-state* #'score nil 4))
;;                              (nth-value 0 (negamax *game-state* #'score T 4)))))

(defun run-unit-tests ()
  (let ((results (lisp-unit:run-tests :all :elo100)))
    (sb-ext::quit :unix-status (+ (length (error-tests results))
                                  (length (failed-tests results))))))

(defun move-list-contains (move-list move)
  (reduce (lambda (success pos-move)
            (or success
                (equalp move pos-move))) move-list :initial-value nil))
