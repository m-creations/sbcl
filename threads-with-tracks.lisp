(in-package :cl-user)

;;
;; Set up some infrastructure to claim a fresh track for each new thread
;;

(defparameter *min-track* 1)
(defparameter *max-track* (1- sb-vm:+reserved-track+)) ;; inclusive

(defvar *track-threads*
  (make-array (1+ *max-track*)
              :initial-element nil
              :fill-pointer *min-track*))

(defun %compiled-function-name (f)
  (let ((s (write-to-string f))
        (prefix "#<FUNCTION ")
        (suffix ">"))
    (assert (uiop:string-prefix-p prefix s))
    (assert (uiop:string-suffix-p s suffix))
    (subseq s (length prefix) (- (length s) (length suffix)))))

(defun make-thread-with-track (tr f &key name arguments)
  (assert (null (aref *track-threads* tr)))
  (let* ((th-name-raw (or name (typecase f
                                 (symbol (symbol-name f))
                                 (compiled-function (%compiled-function-name f))
                                 (t (format nil "[~A]" (type-of f))))))
         (th (make-thread* (lambda (tr* &rest args*)
                             ;; switch to new track & call payload
                             (sb-vm::with-track (tr*)
                               (apply f args*)))
                           :arguments (cons tr arguments)
                           :name (name-with-track tr th-name-raw))))
    (setf (aref *track-threads* tr) th)))

(defun make-thread-with-fresh-track (f &key name arguments)
  (let ((fp (fill-pointer *track-threads*))
        (end (array-total-size *track-threads*)))
    (cond ((< fp end)
           (assert (null (aref *track-threads* fp))) ;; check invariant
           (let ((i (vector-push nil *track-threads*)))
             (assert (= i fp))
             (make-thread-with-track i f :name name :arguments arguments)
             i))
          (t
           ;; TODO: search for unassigned track that is also completely empty
           (cerror "Skip thread creation."
                   "Oops! Sorry, all tracks in use.")))))
