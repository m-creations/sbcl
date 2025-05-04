(in-package :cl-user)

(defun tmpdir ()
  (let* ((tmpdir (sb-ext:posix-getenv "TMPDIR"))
	 (tmpdir (and tmpdir (probe-file tmpdir)))
	 (tmpdir (or tmpdir #P"/tmp/"))
	 (tmpdir (namestring tmpdir)))
    ;; always has trailing slash
    (assert (plusp (length tmpdir)))
    (assert (char= #\/ (char tmpdir (1- (length tmpdir)))))
    tmpdir))

(defvar *tmpfile-prefix* "tmp")

#+nil
(defun tmpfile-pattern (&optional (prefix *tmpfile-prefix*))
  (concatenate 'string (tmpdir) prefix "XXXXXX"))

;;

(defun tmpfile-pattern ()
  "/tmp/tmpXXXXXX")

(defun file-lines (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil nil nil)
	  while line
	  collect line)))

(defun gc-gen-report-to-fd (fd)
  (with-alien ((gc-gen-report-to-file (function void int unsigned) :extern))
    (alien-funcall gc-gen-report-to-file fd 0)))

(defun gc-gen-report-to-lines ()
  (multiple-value-bind (fd f-path) (sb-unix:sb-mkstemp (tmpfile-pattern) #o600)
    (gc-gen-report-to-fd fd)
    (sb-unix:unix-close fd)
    (let ((lines (file-lines f-path)))
      (delete-file f-path)
      lines)))

(defun gc-gen-report-to-string ()
  (format nil "~{~A~%~}" (gc-gen-report-to-lines)))

(defun gc-gen-report (&optional (stream *standard-output*))
  (format stream "~{~A~%~}" (gc-gen-report-to-lines))
  (finish-output stream))
