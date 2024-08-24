(in-package sb-vm)

(export '(track                ;; = type (integer 0 (1- +tracks-end+))
          +track-bits+         ;; = 8
          +tracks-end+         ;; = 256
          +default-track+      ;; = 0
          +unused-track+       ;; = #xff
          +initial-track+      ;; = 0    ;; same as +default-track+
          +private-cons-track+ ;; = #xff ;; same as +unused-track+
          with-track
          call-using-track
          switch-to-track
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

(defun track-of (obj)
  (with-pinned-objects (obj)
    (let ((addr (get-lisp-obj-address obj)))
      (declare (word addr))
      (let ((p (find-page-index addr)))
        (unless (minusp p)
          (deref page-tracks p))))))
