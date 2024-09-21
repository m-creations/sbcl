#-allocation-tracks (invoke-restart 'run-tests::skip-file)

;;;
;;; an inhomogeneous long list, mixed from different tracks
;;;

(defconstant +user-tracks+ (- sb-vm:+tracks-end+ 4))

(defun test-long-list-with-mixed-tracks (n)
  (let ((l nil))
    (dotimes (i (* n 256))
      (loop
        :for tr :from 1 :below +user-tracks+
        :do (sb-vm:with-track (tr)
              (push tr l))))
    (sb-ext:gc :full t)
    (loop
      :with tail := l
      :while tail
      :do
         (assert (= (sb-vm:track-of tail) (car tail)))
         (setf tail (cdr tail)))))

(with-test (:name (:tracks :basic :lists))
  (test-long-list-with-mixed-tracks 100))
