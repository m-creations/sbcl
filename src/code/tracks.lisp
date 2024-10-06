(in-package sb-vm)

(export '(track                ;; = type (integer 0 (1- +tracks-end+))
          +track-bits+         ;; = 8
          +tracks-end+         ;; = 256
          +default-track+      ;; = 0
          +unused-track+       ;; = #xff
          +initial-track+      ;; = 0    ;; same as +default-track+
          +private-cons-track+ ;; = #xff ;; same as +unused-track+
          +reserved-track+     ;; = #xfe
          with-track
          call-using-track
          switch-to-track
          track-pages          ;; = count pages with given track
          track-bytes-used     ;; = sum over track-pages
          track-bytes-wasted   ;; = 32k * pages - used
          ;;track-userdata
          ;;new-track
          ;; ;;unuse-track
          on-same-track
          do-track-pages     ;; assumes a WITHOUT-GCING context
          ;;do-track-objects
          dump-track-objects
          ;; track-contents
          merge-track
          merge-current-track

          destroy-track ;; = mark pages as free & zero them (or mark as such)
          ;;hide-track
          ;;unhide-track
          points-to-track
          ;; c-find-arena->track
          ;; c-find-other-heap->track
          ;; c-find-default-track->track
          ;; show-arena->track
          ;; show-other-heap->track
          ;; show-default-track->track
          ))

(defmacro with-track ((track) &body body)
  (declare (ignorable track))
  #-system-tlabs `(progn ,@body)
  #+system-tlabs
  (let* ((tr-var (gensym "TR"))
         (orig-track-var (gensym "ORIG-TRACK")))
    `(let* ((,tr-var ,track)
            (,orig-track-var (thread-current-track)))
       (declare (track ,tr-var))
       (switch-to-track ,tr-var)
       (unwind-protect (progn ,@body)
         (switch-to-track ,orig-track-var)))))

(defun maybe-show-track-switch (outer-track inner-track direction reason)
  (declare (ignore outer-track inner-track direction reason)))
#+system-tlabs
(defun call-using-track (thunk track reason)
  (declare (track track))
  (let ((orig-track (thread-current-track)))
    (progn
      (maybe-show-track-switch orig-track track "enter" reason)
      (switch-to-track track)
      (multiple-value-prog1 (funcall thunk)
        (switch-to-track orig-track)
        (maybe-show-track-switch orig-track track "leave" reason)))))

(define-alien-variable "page_tracks" (* (unsigned 8)))
(define-alien-variable "page_table" (* (struct page)))

(defun track-of (obj)
  (with-pinned-objects (obj)
    (let ((addr (get-lisp-obj-address obj)))
      (declare (word addr))
      (let ((p (find-page-index addr)))
        (unless (minusp p)
          (deref page-tracks p))))))

(defun on-same-track (obj1 obj2)
  (eql (track-of obj1)
       (track-of obj2)))

;;

(define-alien-variable "next_free_page" sb-kernel::page-index-t)

(defmacro do-pages ((i &optional page) &body body)
  (let ((total-pages (gensym "TOTAL-PAGES")))
    `(let ((,total-pages next-free-page))
       (dotimes (,i ,total-pages)
         (let (,@(when page
                   `((,page (deref page-table ,i)))))
           ,@body)))))

(defmacro do-track-pages ((tr i &optional page) &body body)
  (let ((tr* (gensym "TR")))
    `(let ((,tr* ,tr))
       (do-pages (,i)
         (when (eql ,tr* (deref page-tracks ,i))
           (let (,@(when page
                     `((,page (deref page-table ,i)))))
             ,@body))))))

(defun %track-pages (tr)
  (let ((pages 0))
    (declare (type fixnum pages))
    (do-track-pages (tr i)
      (incf pages))
    pages))

(defun track-pages (tr)
  (without-gcing () ;; is this enough?
    (%track-pages tr)))

#|
(macrolet ((aligned-base (blk)
             `(align-up (sap-int (sap+ ,blk (* 4 n-word-bytes))) 4096)))
|#

(defun dump-track-objects (tr &aux (tot-size 0))
  (declare (ignore tr)) #+nil
  (do-track-pages (tr i page)
    (let ((from (aligned-base memblk))
          (to (sap-int (arena-memblk-freeptr memblk))))
      (format t "~&Page ~6D contents ~X..~X~%" from to)
      (map-objects-in-range
       (lambda (obj type size)
         (declare (ignore type))
         (incf tot-size size)
         (format t "~x ~s~%" (get-lisp-obj-address obj) (type-of obj)))
       (%make-lisp-obj from)
       (%make-lisp-obj to))))
  tot-size)

(defun %track-bytes-used (tr)
  (let ((sum-bytes-used 0))
    (declare (type fixnum sum-bytes-used))
    (do-track-pages (tr i)
      (let* ((page (deref page-table i))
             (words-used* (slot page 'words-used*))
             (bytes-used (ash (ash words-used* -1) word-shift)))
        (incf sum-bytes-used bytes-used)))
    sum-bytes-used))

(defun track-bytes-used (tr)
  (without-gcing () ;; is this enough?
    (%track-bytes-used tr)))

(defun %track-bytes-wasted (tr)
  (- (* (%track-pages tr) gencgc-page-bytes)
     (%track-bytes-used tr)))

(defun track-bytes-wasted (tr)
  (without-gcing () ;; is this enough?
    (%track-bytes-wasted tr)))

;;

(defun merge-track (tr &key (into +default-track+))
  ;; FIXME: figure out appropriate locking
  ;; FIXME: fixup thread->track slots
  ;; FIXME: think about resetting "hint" pages
  (do-track-pages (tr i)
    (setf (deref page-tracks i) into)))

(defun merge-current-track (&key (into +default-track+))
  (merge-track (thread-current-track) :into into))

(defun join-track (tr)
  (merge-track tr :into (thread-current-track)))

;;

(defun %points-to-track (tr addr)
  (let ((p (find-page-index addr)))
    (unless (minusp p)
      (eql tr (deref page-tracks p)))))

(defun points-to-track (tr x)
  (without-gcing ()
    (etypecase x
      (system-area-pointer (%points-to-track tr (sap-int x)))
      (fixnum (%points-to-track tr x)))))

(defun c-find-arena->track ()
  )

(defun c-find-other-heap->track ()
  )

(defun c-find-default-track->track ()
  )

(defun find-track-ptr (x)
  (declare (ignore x))
  (values 0 0)) ;; TBD

(defun show-heap->track (l)
  (dolist (x l)
    (cond ((typep x '(cons sb-thread:thread))
           ;; It's tricky to figure out what a symbol in another thread pointed to,
           ;; so just show the symbol and hope the user knows what it's for.
           (format t "~&Symbol ~/sb-ext:print-symbol-with-prefix/~%" (third x)))
          (t
           (let ((pointee (nth-value 1 (find-track-ptr x))))
             (format t "~x -> ~x ~s ~s~%"
                     (get-lisp-obj-address x)
                     (get-lisp-obj-address pointee)
                     (type-of x)
                     (type-of pointee)))))))

(defun show-arena->track ()
  )

(defun show-other-heap->track ()
  )

(defun show-default-track->track ()
  )

;;

(defun %delete-track-contents (tr)
  "This UNSAFE operation will DELETE ALL objects allocated on track TR.
The caller is responsible for ensuring that there aren't any remaining
pointers from elsewhere to any of those objects. Otherwise bad things
are bound to happen eventually."
  (do-track-pages (tr i)
    (setf (deref page-tracks i) +unused-track+)
    (let ((page (deref page-table i)))
      (setf (slot page 'flags) 0 ;; mark as free
            (slot page 'words-used*) 1)))) ;; need_zerofill

(defun destroy-track (tr)
  (without-gcing () ;; what else?
    (%delete-track-contents tr)))
