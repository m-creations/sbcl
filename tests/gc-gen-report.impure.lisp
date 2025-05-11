(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(defvar *memtop-dir*
  (resolve-file "../contrib/sb-memtop/"))

(defun load* (relpath)
  (load (compile-file (merge-pathnames relpath *memtop-dir*))))

;; Wrapper for C-level report, usually written to (sb-ext:gc-logfile)
;; before and after GC (if enabled).
(load* "gc-gen-report-wrapper.lisp")

#|
(in-package :sb-debug)

#+nil
(defmacro with-world-stopped-for ((purpose &key) &body body)
  `(let ((purpose ,purpose))
     (declare (type (not null) purpose)
              (ignorable purpose))
     (without-gcing
         (when (sb-kernel::try-acquire-gc-lock
              (sb-kernel::gc-stop-the-world))
           (unwind-protect
               (progn ,@body)
             (sb-kernel::gc-start-the-world))))))
|#

;; LISP-level implementation
(load* "gc-gen-report-to-stream.lisp")

(require 'uiop)

(with-test (:name :gc-gen-report)
  (let (s1 s2)
    (setf s1 (with-output-to-string (s)
               (sb-vm::gc-gen-report-to-stream s)
               (setf s2 (cl-user::gc-gen-report-to-string))))
    (let ((diffs@pos (loop :for i :below (length s1)
                           :unless (char= (char s1 i) (char s2 i))
                             :collect i)))
      (assert (null diffs@pos)))
    (assert (uiop:string-prefix-p s1 s2))))
