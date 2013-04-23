(in-package :elo100)

(defun random-move ()
  (let ((moves (possible-moves)))
    (nth (random (length moves)) moves)))

(defun play-game (white-player black-player)
  (labels ((innergame ()
             (if (eql *game-status* :ongoing)
                 (let ((move (funcall (if (eql (getf *state* :on-move) :white) white-player black-player))))
                   (print-state *state*)
                   (with-move move
                     (innergame)))
                 *game-status*)))
    (with-state (make-state) (innergame))))

(defun play-random-game ()
  (play-game #'random-move #'random-move))

(defun play-bot-vs-random-game ()
  (play-game #'bot-move #'random-move))

(defun play-random-vs-bot-game ()
  (play-game #'random-move #'bot-move))
