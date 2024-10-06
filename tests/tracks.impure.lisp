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

(sb-ext:gc :full t)


;;;
;;; TRACK-SELECTION encoding, preferably as fixnum
;;;


(defun %try-encode-tracks/fixnum-bits (tracks)
  ;; assumes non-nil tracks, returns nil on failure
  (loop
    :with n-bits := (1- sb-vm:n-fixnum-bits)
    :with result := most-negative-fixnum
    :for tr :in tracks
    :do
       (if (< tr n-bits)
           (setf (logbitp tr result) t)
           (return nil)) ;; cannot encode
    :finally
       (return result)))

(defun %try-encode-tracks/fixnum-bytes (tracks)
  ;; assumes non-nil tracks, returns nil on failure
  (loop
    :with result := 0
    :for tr :in (sort tracks '<)
    :for shift :from 0 :by sb-vm:+track-bits+
    :do (incf result (ash tr shift))
    :finally
       (when (typep result 'fixnum)
         (return result))))

(defun encode-tracks/bit-vector (tracks)
  (let ((bv (make-array sb-vm:+tracks-end+ :element-type 'bit)))
    (dolist (tr tracks)
      (setf (aref bv tr) 1))
    bv))

(defun encode-tracks (tracks)
  (when tracks
    (or (%try-encode-tracks/fixnum-bits tracks)
        (%try-encode-tracks/fixnum-bytes tracks)
        (encode-tracks/bit-vector tracks))))

(with-test (:name (:tracks :selection-encoding :fixnum-bytes))
  ;; small sets of arbitrary tracks, as a non-negative fixnum
  (assert (= 5                  (%try-encode-tracks/fixnum-bytes '(5))))
  (assert (= #x1305             (%try-encode-tracks/fixnum-bytes '(#x05 #x13))))
  #+64-bit
  (assert (= #x3f06050403020100 (%try-encode-tracks/fixnum-bytes '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x3f))))
  (assert (null                 (%try-encode-tracks/fixnum-bytes '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x40))))
  (assert (null                 (%try-encode-tracks/fixnum-bytes '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8)))))

(with-test (:name (:tracks :selection-encoding :fixnum-bits))
  ;; medium sets of low-numbered tracks, as a negative fixnum
  (assert (= (+ most-negative-fixnum #b100000)            (%try-encode-tracks/fixnum-bits '(5))))
  (assert (= (+ most-negative-fixnum #x80020)             (%try-encode-tracks/fixnum-bits '(#x05 #x13))))
  #+64-bit
  (assert (= (+ most-negative-fixnum #x200000000000007f)  (%try-encode-tracks/fixnum-bits '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x3d))))
  (assert (null                                           (%try-encode-tracks/fixnum-bits '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x40))))
  (assert (= (+ most-negative-fixnum #x1ff)               (%try-encode-tracks/fixnum-bits '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8)))))


;;;
;;; WALK-DYNAMIC-SPACE with TRACK-SELECTION arg
;;;

(defun count-track-objects (track-selection)
  (let ((count 0))
    (sb-sys:without-gcing ()
      (sb-vm::walk-dynamic-space
       (lambda (&rest args)
         (declare (ignore args))
         (incf count))
       #b111111 0 0 track-selection))
    count))

(defun test-track-count (tr expected-count)
  (flet ((_test/0 (track-selection)
           (unless (zerop (count-track-objects track-selection))
             (format *trace-output* "~&; track-selection = ~S~%" track-selection))
           (assert (zerop (count-track-objects track-selection))))
         (_test (track-selection)
           (unless (= expected-count (count-track-objects track-selection))
             (format *trace-output* "~&; track-selection = ~S~%" track-selection))
           (assert (= expected-count (count-track-objects track-selection)))))
    (let* ((range-with-tr (loop :for i :from 1 :upto 7 :collect i))
           (range-without-tr (set-difference range-with-tr (list tr))))
      (_test tr)                         ;; single track, simple fixnum
      (_test/0 (1+ tr))
      (_test (lambda (x) (eql x tr)))    ;; test predicate, simple function
      (_test/0 (lambda (x) (member x `(,(1- tr) ,(1+ tr)))))
      (_test `(,tr))                     ;; list of tracks
      (_test/0 range-without-tr)
      (_test `((,tr . ,tr)))             ;; ... or track ranges
      (_test/0 `((1 . ,(1- tr)) (,(1+ tr) . 252)))
      (_test (+ (ash tr sb-vm:+track-bits+) 1)) ;; equivalent to `(,tr 1)
      (_test/0 (encode-tracks range-without-tr))
      (_test (%try-encode-tracks/fixnum-bits `(,tr)))
      (_test/0 (%try-encode-tracks/fixnum-bits range-without-tr))
      (_test (encode-tracks/bit-vector `(,tr)))
      (_test/0 (encode-tracks/bit-vector range-without-tr)))))

(with-test (:name (:tracks :walk-dynamic-space 1))
  (let* ((tr 5)
         (foo (sb-vm:with-track (tr)
                (make-array 1000))))
    (test-track-count 5 1)
    foo))

(sb-ext:gc :full t)

(with-test (:name (:tracks :walk-dynamic-space 2))
  (test-track-count 5 0))
