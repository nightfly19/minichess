(when (not (find-package "QL"))
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(when (not (find-package "QL"))
  (load "/usr/share/cl-quicklisp/quicklisp.lisp"))

(when (not (find-package "QL"))
  (load "./.bootstrap-test-2.lisp"))
