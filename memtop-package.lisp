
(in-package :cl-user)

(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(load (compile-file (resolve-file "gc-gen-report-wrapper.lisp")))
(load (compile-file (resolve-file "gc-gen-report-to-stream.lisp")))

(defmacro try-form (form)
  `(handler-case
       (progn
         (format *error-output* "~&; ~S ... " ',form)
         (finish-output *error-output*)
         ,form
         (format *error-output* "DONE~%"))
     (serious-condition (e)
       (format *error-output* "FAILED: ~S~%" e))))

(require 'sb-bsd-sockets)
(use-package :sb-bsd-sockets)

(try-form (require 'uiop))

#|
(unless (find-package :quicklisp)
  (try-form (load "~/quicklisp/setup.lisp")))

#+quicklisp
(ql:quickload :trivial-raw-io)
|#
