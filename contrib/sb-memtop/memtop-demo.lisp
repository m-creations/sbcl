#!/bin/bash -e
#|
../../run-sbcl.sh --script "$0"
exit 0
|#

(in-package :cl-user)

(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(load (resolve-file "memtop.lisp"))
(load (resolve-file "threads-with-tracks.lisp"))

(setf sb-vm::*gc-tracks-report-show-thread-names-p* t)

;;

(defvar *gc-logfile-name*
  "/tmp/gc-logfile.txt")

(def-memtop-extension #\L ()
  ;; toggle gc logging to file
  (setf (sb-ext:gc-logfile)
    (if (sb-ext:gc-logfile)
        nil
        *gc-logfile-name*)))

;; some dummy workers

#| example:

|#

(defvar *worker-configs* nil)

(defmacro def-worker (worker-fun (roots-var
                                  &key
                                      char
                                      (throttle 0.001)
                                      (reset-interval 10.0))
                      &body body)
  (macrolet ((sym! (&rest things)
               `(sb-int:symbolicate ,@things)))
    (let ((roots-name* (symbol-name roots-var)))
      (assert (char= #\* (char roots-name* 0)))
      (assert (char= #\* (char roots-name* (1- (length roots-name*)))))
      (let* ((roots-name (subseq roots-name* 1 (1- (length roots-name*))))
             (period-var (sym! "*RESET-" roots-name "-PERIOD*"))
             (reset-fun  (sym! "RESET-" roots-name "-LOOP"))
             (throttle-var (sym! "*THROTTLE-" roots-name "*")))
        (assert (search roots-name (symbol-name worker-fun)))
        (let ((c (or char (char roots-name 0))))
          `(progn
             ;;
             (defvar ,roots-var nil)
             (defvar ,throttle-var ,throttle)
             (defvar ,period-var ,reset-interval)
             ;;
             (defun ,reset-fun ()
               (loop
                 (sleep ,period-var)
                 (setf ,roots-var nil)))
             ;;
             (defun ,worker-fun ()
               (loop
                 (sleep ,throttle-var)
                 ,@body))
             ;;
             (def-memtop-extension ,c ()
               (make-thread-with-fresh-track (function ,worker-fun)))
             ;;
             (let ((config `(:fun ,',worker-fun :char ,,c :throttle ,',throttle-var
                             :periodical-reset-fun ,',reset-fun :reset-period ,',period-var)))
               (push config *worker-configs*))))))))

(def-worker alloc-lists (*lists*)
  (sb-ext:atomic-push (make-list 254) *lists*))

(def-worker alloc-arrays (*arrays*)
  (sb-ext:atomic-push (make-array 510) *arrays*))

(def-worker alloc-hash-tables (*hash-tables*)
  (let* ((n 50 #+nil (random 100))
         (ht (make-hash-table :size (1- (truncate (* n 1.5))))))
    (sb-ext:atomic-push ht *hash-tables*)
    (loop :for i :below n
          :do (setf (gethash i ht) (* i i)))))

;;

(def-memtop-extension #\w (io-stream)
  ;; launch a specific worker (designated by the next input char) on a fresh track
  (let ((c #\A #+nil (read-char* io-stream)))
    (case c
      ((#\return #\newline) ;; ignore
       (setf c (read-char* io-stream))))
    (assert c)
    (let ((config (find c *worker-configs*
                        :key (lambda (config) (getf config :char))
                        :test 'eql)))
      (assert config)
      (let ((f (getf config :fun)))
        (declare (type function f))
        (make-thread-with-fresh-track f)))))

(def-memtop-extension #\+ ()
  ;; launch a random worker on a fresh track
  (let* ((worker-config (nth (random (length *worker-configs*)) *worker-configs*))
         (worker-fun (getf worker-config :fun)))
    (assert (functionp worker-fun))
    (make-thread-with-fresh-track worker-fun)))

(defun kill-thread (th)
  (when (sb-thread:thread-alive-p th)
    (sb-thread:interrupt-thread th #'sb-thread:abort-thread)
    (loop
      :do (sleep 0.001)
      :while (sb-thread:thread-alive-p th))))

(defun kill-thread-on-track (tr)
  (assert (typep tr `(integer ,*min-track* ,*max-track*)))
  (let ((th (aref *track-threads* tr)))
    (kill-thread th)))

(def-memtop-extension #\k (io-stream)
  ;; kill the worker associated with the given track
  (let ((tr (parse-integer (read-line io-stream nil nil nil))))
    (kill-thread-on-track tr)))

(def-memtop-extension #\- ()
  ;; kill some worker thread
  (loop
    :for tr :from (1- (fill-pointer *track-threads*)) :downto *min-track*
    :for th := (aref *track-threads* tr)
    :until (and th (sb-thread:thread-alive-p th))
    :finally
       (when th
         (kill-thread th))))

(loop
  :for config :in *worker-configs*
  :for reset-fun := (getf config :periodical-reset-fun)
  :do (make-thread* (symbol-function reset-fun) :name (symbol-name reset-fun)))

(format t "Worker configs:~{~%(~{~S ~S~^~% ~})~}~%" *worker-configs*)
(serve-memtop)
