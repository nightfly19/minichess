(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "lisp-unit"))

(load "package.lisp")
(load "minichess.lisp")
(load "bot.lisp")
(load "external.lisp")
(load "tests.lisp")
