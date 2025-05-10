(in-package :cl-user)

(defun resolve-file (path)
  (merge-pathnames path (or *load-pathname* *compile-file-pathname*)))

(load (resolve-file "memtop-package.lisp"))

#+asdf
(let (s1 s2)
  ;; self-test ...
  (setf s1 (with-output-to-string (s)
             (sb-vm::gc-gen-report-to-stream s)
             (setf s2 (cl-user::gc-gen-report-to-string))))
  (let ((diffs@pos (loop :for i :below (length s1)
                         :unless (char= (char s1 i) (char s2 i))
                           :collect i)))
    (assert (null diffs@pos)))
  (assert (uiop:string-prefix-p s1 s2)))

(defvar *memtop-port* 9999)
(defvar *memtop-seconds* 1)
(defvar *memtop-summary-p* t)
(defvar *memtop-summary-from-c-p* nil) ;; unsafe; may crash the server process!
(defvar *memtop-room-track* nil)
(defvar *memtop-room-lines* 20)
(defvar *memtop-quit* nil)

(defmacro with-broken-pipe-handler ((&body handler-body) &body body)
  `(catch 'done
     (handler-bind ((sb-int:broken-pipe
                     #'(lambda (e)
                         (declare (ignorable e))
                         ,@handler-body
                         (throw 'done nil))))
       ,@body)))

;; #+quicklisp
(defun interactive (&optional (io-stream *query-io*))
  (with-broken-pipe-handler ((format *error-output* "~&; *** interactive encountered broken-pipe~%"))
    (loop :while T :do
      (handler-case
          (let ((c #-quicklisp (read-char io-stream nil nil nil)
                   #+quicklisp (trivial-raw-io:read-char io-stream)))
            (case c
              ((#\return #\newline)) ;; ignore
              ((#\S) ;; toggle report summary
               (setf *memtop-summary-p*
                 (not *memtop-summary-p*)))
              ((#\c) ;; collect garbage (GC)
               (sb-ext:gc))
              ((#\g) ;; (gc :gen n)
               (sb-ext:gc :gen (parse-integer (read-line io-stream))))
              ((#\F) ;; full GC
               (sb-ext:gc :full t))
              ((#\s) ;; seconds
               (setf *memtop-seconds* (parse-integer (read-line io-stream))))
              ((#\t) ;; track
               (let* ((line (read-line io-stream)))
                 (setf *memtop-room-track*
                   (if (every #'sb-unicode:whitespace-p line)
                       nil
                       (parse-integer line)))))
              #+nil
              ;; When the client reports "Conection closed by foreign host.",
              ;; an immediately following server restart will fail with
              ;;   Socket error in "bind": EADDRINUSE (Address already in use)
              ((#\q) ;; quit
               (setf *memtop-quit* t)
               (loop-finish))
              ((#\C #\L) ;; toggle summary generation from C (unsafe) vs Lisp
               (setf *memtop-summary-from-c-p*
                 (not *memtop-summary-from-c-p*)))
              (t
               (format io-stream "~&; *** unexpected input character: ~S~%" c))))
        (serious-condition (e)
          (format io-stream "~&; *** ERROR: ~S~%" e))))))

(defun memtop/room-objects (tr &key callback/before callback/after)
  (sb-vm::with-track (sb-vm::+reserved-track+)
    (let* ((ht (make-hash-table :test 'equal)))
      (flet ((_fun (obj widetag size)
               (let ((key (list widetag (type-of obj) size)))
                 ;; We're without-gcing, so first try if this key exists already ...
                 (declare (dynamic-extent key))
                 (let ((val (gethash key ht)))
                   (if val
                       (incf (gethash key ht))
                       ;; ... no luck, so we have to cons it for real
                       (let ((key* (copy-list key)))
                         (setf (gethash key* ht) 1)))))))
        (sb-sys::without-gcing
          (sb-vm::walk-dynamic-space #'_fun #b1111111 0 0 tr)))#|)|#
      (let ((by-count nil))
        (maphash (lambda (k v)
                   (push (cons v k) by-count))
                 ht)
        (setf by-count (stable-sort by-count '> :key (lambda (entry)
                                                       (let ((count (car entry))
                                                             (size (third (cdr entry))))
                                                         (* count size)))))
        (when callback/before
          (funcall callback/before))
        (loop
          :initially
             (format t "~& ~15@A  ~15@A  ~18@A  ~3@A  ~A~%"
                     "COUNT" "SIZE" "BYTES" "WT" "TYPE")
          :for line :below *memtop-room-lines*
          :for (count widetag lisp-type size) :in by-count
          :do (format t "~& ~15:D  ~15:D  ~18:D  x~2,'0X  ~A~%"
                      count size (* count size) widetag (format nil "~S" lisp-type)))
        (when callback/after
          (funcall callback/after))))))

(defun memtop/gc-gen-report ()
  "Returns the gc_gen_report as a string."
  (if *memtop-summary-from-c-p*
      (cl-user::gc-gen-report-to-string)
      (with-output-to-string (s)
        (sb-sys::without-gcing
            (sb-vm::gc-gen-report-to-stream s)))))

(defun memtop (&aux summary-report)
  (labels ((_intro ()
             ;; clear screen
             (write-string #.(format nil "~C[H~C[2J" #\Escape #\Escape))
             ;; print date & time
             (multiple-value-bind (ss mm hh day month year)
                                  (decode-universal-time (get-universal-time))
               (format t "~A-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hh mm ss))
             ;; print track
             (format t "~@[  -- Track: #x~2,'0X~]" *memtop-room-track*)
             (format t "~%~%"))
           (_intro+summary ()
             (_intro)
             (when summary-report
               (write-string summary-report))))
    (loop :until *memtop-quit* :do
      (setf summary-report
        (when *memtop-summary-p*
          (memtop/gc-gen-report)))
      (let ((tr *memtop-room-track*))
        (if tr
            (memtop/room-objects tr :callback/before #'_intro+summary)
            (_intro+summary)))
      (finish-output)
      #+nil
       (sb-debug::with-world-stopped-for (:memdump)
         (cl-user::gc-gen-report))
       (sleep *memtop-seconds*))))

(defun serve-memtop ()
  (let ((listen-sock (make-instance 'inet-socket :type :stream :protocol :tcp))
        server-sock port)
    (unwind-protect
        (with-broken-pipe-handler ((format *debug-io* "~&; *** ~A~%" 'sb-int:broken-pipe))
          (handler-case
              (socket-bind listen-sock #(127 0 0 1) *memtop-port*)
            (address-in-use-error (e)
              (format *error-output*
                      "~&; *** Port ~D in use, trying alternatives ...~%" *memtop-port*)
              (socket-bind listen-sock #(127 0 0 1) 0)))
          (setf port (nth-value 1 (socket-name listen-sock)))
          (format *debug-io* "~&; memtop server started on port ~D, now run:~%" port)
          (format *debug-io* "~&~%telnet localhost ~D~%~%" port)
          (socket-listen listen-sock 1)
          (setf server-sock (socket-accept listen-sock))
          ;; Wait for input. This should return when we get EOF
          ;; from the client. It should /not/ hang.
          (sb-sys:wait-until-fd-usable (socket-file-descriptor server-sock) :output)
          (let ((s (socket-make-stream server-sock :input t :output t)))
            (listen s)
            (let ((*standard-output* s))
              ;; #+quicklisp
              (sb-thread:make-thread #'interactive :arguments (list s))
              (memtop)
              #+nil
              (loop :for line := (read-line s nil nil nil)
                    :while line
                    :do (format t "          (~{~S~^ ~})~%~C[F~A~%"
                                (loop :for c :across line :collect c)
                                #\escape (length line))
                        (finish-output)))))
      (with-broken-pipe-handler ((format *debug-io* "~&; *** listen-sock: broken-pipe~%"))
        (socket-close listen-sock))
      (when server-sock
        (with-broken-pipe-handler ((format *debug-io* "~&; *** server-sock: broken-pipe~%"))
          (socket-close server-sock))))))
