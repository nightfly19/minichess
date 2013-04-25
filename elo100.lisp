(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "lisp-unit")
  (ql:quickload "cl-ppcre"))

(load "package.lisp")
(load "minichess.lisp")
(load "smallify.lisp")
(load "bot.lisp")
(load "external.lisp")
(load "play.lisp")
(load "imcs.lisp")
(load "tests.lisp")
