
(in-package :cl-user)

;; utility wrapper for setting the OS thread name

(defun set-current-os-thread-name (new-name)
  "Sets OS-level thread name to given string.
  Currently limited to graphic ASCII characters."
  (loop :for c :across new-name
        :do (assert (<= 32 (char-code c) 126)))
  (let* ((th sb-thread:*current-thread*)
         (tid (sb-thread:thread-os-tid th))
         (os-name new-name))
    (with-open-file (out (format nil "/proc/self/task/~D/comm" tid)
                     :external-format :utf-8
                     :direction :output
                     :if-exists :overwrite)
      (write-string os-name out))))

(defparameter *set-os-thread-name-p* t)

(defun name-with-track (tr name)
  (format nil "~2,'0X:~A" tr name))

(defun name-with-current-track (name)
  (name-with-track (sb-vm:thread-current-track) name))

(defun make-thread* (f &key name arguments
                            (os-name
                             (or name
                                 (when *set-os-thread-name-p*
                                   (name-with-current-track "<anon>")))))
  (sb-thread:make-thread (lambda (&rest args)
                           (when *set-os-thread-name-p*
                             (set-current-os-thread-name os-name))
                           (apply f args))
                         :name name
                         :arguments arguments))
