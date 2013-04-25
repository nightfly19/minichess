(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "lisp-unit")
  (ql:quickload "cl-ppcre"))

(load "package.lisp") ;;works
(load "pieces.lisp") ;;works
(load "coords.lisp") ;;works
(load "game-state.lisp") ;;seems to work
(load "possible-moves.lisp") ;;works
(load "game-status.lisp") ;;works
(load "transposition-table.lisp") ;;works
(load "bot.lisp") ;;works
(load "external.lisp") ;;works?
;;(load "play.lisp") ;;works?
(load "imcs.lisp") ;;works
(load "tests.lisp") ;;works, not not good enough :/
