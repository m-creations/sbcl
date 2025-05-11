#!/bin/bash -e
#|
./run-sbcl.sh --script "$0"
exit 0
|#

(in-package :cl-user)

(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(load (resolve-file "memtop.lisp"))
(load (resolve-file "threads-with-tracks.lisp"))

(setf sb-vm::*gc-tracks-report-show-thread-names-p* t)

;; some dummy payloads

(defvar *lists* nil)
(defvar *arrays* nil)
(defvar *hash-tables* nil)

(defun periodical-cleanup ()
  (loop
    (sleep 10)
    (setf *lists* nil
          *arrays* nil
          *hash-tables* nil)))

(defun alloc-lists ()
  (loop
    (sleep 0.001)
    (sb-ext:atomic-push (make-list 254) *lists*)))

(defun alloc-arrays ()
  (loop
    (sleep 0.001)
    (sb-ext:atomic-push (make-array 510) *arrays*)))

(defun alloc-hash-tables ()
  (loop
    (sleep 0.001)
    (let ((ht (make-hash-table)))
      (sb-ext:atomic-push ht *hash-tables*)
      (loop :for i :below (random 100)
            :do (setf (gethash i ht) (* i i))))))

(def-memtop-extension #\+ ()
  (let ((f (case (1+ (random 3))
             ((1) #'alloc-lists)
             ((2) #'alloc-arrays)
             ((3) #'alloc-hash-tables))))
    (make-thread-with-fresh-track f)))

(def-memtop-extension #\- ()
  (loop
    :for i :from (1- (fill-pointer *track-threads*)) :downto *min-track*
    :for th := (aref *track-threads* i)
    :until (and th (sb-thread:thread-alive-p th))
    :finally
       (when th
         (assert (<= *min-track* i *max-track*))
         (assert (sb-thread:thread-alive-p th))
         (sb-thread:interrupt-thread th #'sb-thread:abort-thread)
         (loop
           :do (sleep 0.001)
           :while (sb-thread:thread-alive-p th)))))

;;(setf (sb-ext:gc-logfile) "/tmp/gc-logfile.txt")
(make-thread* #'periodical-cleanup :name "cleanup")
(make-thread-with-fresh-track #'alloc-lists)
(make-thread-with-fresh-track #'alloc-arrays)
(make-thread-with-fresh-track #'alloc-hash-tables)
(serve-memtop)
