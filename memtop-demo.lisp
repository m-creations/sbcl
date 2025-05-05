#!/bin/bash -e
#|
./run-sbcl.sh --script "$0"
exit 0
|#

(in-package :cl-user)

(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(load (resolve-file "memtop.lisp"))

(defvar *mem1* nil)
(defvar *mem2* nil)
(defvar *mem3* nil)

(defun t0/periodical-cleanup ()
  (loop
    (sleep 25)
    (setf *mem1* nil
          *mem2* nil
          *mem3* nil)))

(defun t1/lists ()
  (sb-vm::with-track (1)
    (loop
      (sleep 0.001)
      (push (make-list 254) *mem3*))))

(defun t2/arrays ()
  (sb-vm::with-track (2)
    (loop
      (sleep 0.001)
      (push (make-array 510) *mem1*))))

(defun t3/hash-tables ()
  (sb-vm::with-track (3)
    (loop
      (sleep 0.001)
      (let ((ht (make-hash-table)))
        (push ht *mem2*)
        (loop :for i :below (random 100)
              :do (setf (gethash i ht) (* i i)))))))

;;(setf (sb-ext:gc-logfile) "/tmp/gc-logfile.txt")
(sb-thread:make-thread #'t0/periodical-cleanup)
(sb-thread:make-thread #'t1/lists)
(sb-thread:make-thread #'t2/arrays)
(sb-thread:make-thread #'t3/hash-tables)
(serve-memtop)
