
(in-package :cl-user)

(setf (gc-logfile) "/tmp/gc-log.txt")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "PAGE-TRACKS" :sb-vm)
    (pushnew :allocation-tracks *features*)))

(assert (member :64-bit *features*))

(in-package :sb-vm)

#+mark-region-gc
(progn
  ;;
  (defconstant +single-object-flag-bit+ 4)
  (defconstant +single-object-flag+ 16)
  (assert (= n-lowtag-bits 4))
  (defconstant +line-size+ (ash n-lowtag-bits 8))
  ;;(defconstant +line-shift+ (+ 8 2))
  ;;(define-alien-variable line-bytemap (* (unsigned 8)))
  )

#+:mark-region-gc
(defun page-gen (p)
  (let* ((page (deref page-table p))
         (flags (slot page 'flags)))
    (if (logbitp +single-object-flag-bit+ flags)
        (slot page 'gen)
        (with-alien ((gc-gen-of (function char unsigned int) :extern)
                     (address-line (function (* int) unsigned) :extern))
          (loop
            :with min := 6
            :with max := 0
            :with default := 127
            :for offset :from 0 :by +line-size+ :below gencgc-page-bytes
            :for result := (alien-funcall gc-gen-of (+ dynamic-space-start
                                                       (* gencgc-page-bytes p)
                                                       offset)
                                          default)
            :do (unless (= result default)
                  (setf min (min min result)
                        max (max max result)))
            :finally (return
                       (cond ((= min max) min)
                             ((< min max) (cons min max))
                             (t nil))))))))

#-:allocation-tracks
(defmacro with-track ((tr) &body body)
  (declare (ignore tr))
  `(progn ,@body))

(declaim (inline page-track))
(defun page-track (p)
  (declare (ignorable p))
  #+:allocation-tracks (deref page-tracks p)
  #-:allocation-tracks 0)

(sb-int:defconstant-eqx +page-bits-to-kind+
    '((1 . :unboxed)
      (2 . :boxed)
      (3 . :mixed)
      (4 . :small-mixed)
      (5 . :cons)
      (6 . :err)
      (7 . :code))
  #'equal)

(defun page-bits-to-kind (bits)
  (cdr (assoc (logand bits 7) +page-bits-to-kind+)))

(unless (fboundp 'do-pages)
  (defmacro do-pages ((i page) &body body)
    (let ((total-pages (gensym "TOTAL-PAGES")))
      `(let ((,total-pages sb-vm:next-free-page))
         (dotimes (,i ,total-pages)
           (let ((,page (deref page-table ,i)))
             ,@body))))))

(defparameter *dynamic-col-keys*
  #(((:boxed)       "Boxed")
    ((:cons)        "Cons")
    ((:unboxed)     "Raw")
    ((:code)        "Code")
    #-:mark-region-gc
    ((:small-mixed) "SmMix")
    ((:mixed)       "Mixed")
    #+:mark-region-gc
    ((:boxed . t)   "LgBox")
    ((:unboxed . t) "LgRaw")
    ((:code . t)    "LgCode")
    ((:mixed . t)   "LgMix")
    ))

(defun dynamic-col-index (kind large-p)
  ;;(+ 1 (length immobile-col-keys)
  (position (cons kind large-p) *dynamic-col-keys*
            :key 'car
            :test 'equal))

(defun show-tracks ()
  (let ((track-page-counts nil))
    (do-pages (i page)
      (let ((track (page-track i))
            (gen   #-:mark-region-gc (slot page 'gen)
                   #+:mark-region-gc (page-gen i))
            (bits  (slot page 'flags)))
        (unless (zerop bits) ;; empty page
          (when t ;;(plusp track)
            (let ((counts (cdr (assoc track track-page-counts))))
              (unless counts
                (setf counts ;; an array indexed by gen and bits
                  (make-array (list (+ +pseudo-static-generation+ 2) 256)
                              :initial-element 0))
                (push (cons track counts) track-page-counts))
              (incf (aref counts
                          (if (listp gen) ; (min . max) or NIL
                              (1+ +pseudo-static-generation+) ; different gens
                              gen)
                          bits)))))))
    (setf track-page-counts
      (sort track-page-counts '< :key 'car))
    ;;
    (flet ((_separation-line (&optional (sep-char #\-))
             (flet ((_sep-chars (n)
                      (make-string n :initial-element sep-char)))
               (format t "~&~A  ~A~%"
                       (_sep-chars 16)
                       (_sep-chars (1- (* 7 (length *dynamic-col-keys*))))))))
      (loop
        :initially (_separation-line #\=)
        :with header-done-p := nil
        :with separation-line-done-p := nil
        :for (track . counts) :in track-page-counts
        :do (loop
              :initially
                 (when (and (= track #xfe)
                            (not separation-line-done-p))
                   (_separation-line)
                   (setf separation-line-done-p t))
              ;;:with header-done-p := nil
              :for gen :below (+ +pseudo-static-generation+ 2)
              :for counts-per-bits := (loop
                                        :for bits :below 256
                                        :collect (aref counts gen bits))
              :unless (or ;;(= gen +pseudo-static-generation+)
                          (every #'zerop counts-per-bits))
                :do
                   (unless header-done-p
                     (format t "                  ~{~6@A~^ ~}~%"
                             (loop :for spec :across *dynamic-col-keys* :collect (second spec)))
                     (setf header-done-p t))
                   ;;
                   (format t "track ~2X, gen ~C:  ~{~6D~^ ~}~%"
                           track
                           (if (<= gen +pseudo-static-generation+)
                               (code-char (+ (char-code #\0) gen))
                               #\*)
                           (loop
                             :with output-cols := (make-array (length *dynamic-col-keys*))
                             :for bits :from 0
                             :for count :in counts-per-bits
                             :for kind := (page-bits-to-kind bits)
                             :for large-p := (logtest bits 16)
                             :for col-index := (dynamic-col-index kind large-p)
                             :if col-index
                               :do (incf (aref output-cols col-index) count)
                             :else
                               :when (plusp count)
                                 :do (format t "~&; *** no col entry found for bits = ~S, kind = ~S, large-p = ~S~%" bits kind large-p)
                             :finally
                                (return
                                  (loop :for idx :below (length output-cols)
                                        :collect (aref output-cols idx)))))
                   (setf separation-line-done-p nil)
              :finally
                 (when (zerop track)
                   (_separation-line)
                   (setf separation-line-done-p t)))

        :finally
           (terpri)
           (finish-output)))))

;;;
;;; Demo
;;;

(defun alloc-pages/cons (n-pages)
  ;; a long list
  (let* ((n-bytes (* n-pages (- gencgc-page-bytes 16)))
         (n-conses (truncate n-bytes 16)))
    (make-list n-conses :initial-element 0)))

(defun alloc-pages/boxed (n-pages)
  ;; lots of smaller arrays
  (let* ((n-bytes (* n-pages gencgc-page-bytes))
         (n-elems 30)
         (array-bytes (+ 16 (* n-word-bytes n-elems)))
         (n-arrays (truncate n-bytes array-bytes))
         (tmp nil))
    (dotimes (i n-arrays)
      (setf tmp
        (let ((leaf (make-array n-elems :initial-element nil)))
          (setf (aref leaf 0) tmp)
          leaf)))
    tmp))

(defun alloc-pages/large-mixed (n-pages)
  ;; a large array
  (let* ((n-bytes (* n-pages gencgc-page-bytes)) ; 32k pages
         (n-elems (truncate (- n-bytes 16) n-word-bytes))) ; 8 bytes per element
    (make-array n-elems)))

(defun %demo ()
  (loop
    :initially (gc :full t)
    :with last-gen := 5
    :with tmp-cons   := (make-array (1+ last-gen))
    :with tmp-boxed  := (make-array (1+ last-gen))
    :with tmp-lg-mix := (make-array (1+ last-gen))
    :for tr :from last-gen :downto 1
    :do
       ;; allocate some pages in track TR
       (with-track (tr)
         (setf (aref tmp-cons tr)
           (alloc-pages/cons (* tr 10)))
         (setf (aref tmp-boxed tr)
           (alloc-pages/boxed (* tr 20)))
         (setf (aref tmp-lg-mix tr)
           (alloc-pages/large-mixed (* tr 30))))
       ;; push them down to gen TR
       (gc :gen tr)
    :finally
       (format t "~%After allocating ~~10x Cons, ~~20x Boxed, ~~30x LgMix pages:~%")
       (show-tracks)
       ;;
       (gc :full t) (format t "~%After performing a full GC:~%")
       (show-tracks)
       (return (list
                :cons tmp-cons
                :boxed tmp-boxed
                :lg-mix tmp-lg-mix))))

(defun demo ()
  (%demo)
  (gc :full t)
  (format t "~%After clearing all (incl full GC):~%")
  (show-tracks)
  (values))

(format t "~&~%;; *** Try running (demo) ...~%")
(import 'show-tracks :cl-user)
(import 'demo :cl-user)

(import '%demo :cl-user)

#|

./run-sbcl.sh --load tracks-demo.lisp --eval '(defvar *foo* (%demo))' --eval '(save-lisp-and-die "/tmp/tracks-demo.core")' && ./run-sbcl.sh --core /tmp/tracks-demo.core --eval '(show-tracks)'

|#
