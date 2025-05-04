
(in-package :sb-vm)

(deftype uintptr-t ()
  `(unsigned-byte 64))

(deftype uword-t ()
  '(unsigned-byte 64))

(defmacro range (min max)
  `(loop :for i :from ,min :upto ,max :collect i))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;
  (defun page-index-fmt (width)
    (format nil "~~~DD" width))
  ;;
  (defun os-vm-size-fmt (&optional width)
    (format nil "~~~@[~D~]D" width)))

#|
os_vm_size_t large_allocation = 0       ;
int n_lisp_gcs;

/* the verbosity level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1. */
int gencgc_verbose = 0;

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
/* an array of track structures. */
struct track tracks[TRACKS_END];
#endif

/* an array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generation is temporarily raised then lowered. */
struct generation generations[NUM_GENERATIONS];
|#

#+nil
(define-alien-variable gencgc-oldest-gen-to-gc (signed 8)) ;; default = +highest-normal-generation+
;; next-free-page ; = upper (exclusive) bound on used page range

(define-symbol-macro free-page-flag 0)
(define-symbol-macro page-type-unboxed 1)
(define-symbol-macro page-type-boxed 2)
(define-symbol-macro page-type-mixed 3)
(define-symbol-macro page-type-small-mixed 4)
(define-symbol-macro page-type-cons 5)
(define-symbol-macro page-type-code 7)
#|
__attribute__((unused)) static const char * const page_type_description[8] =
  {0, "unboxed", "boxed", "mixed", "sm_mix", "cons", "?", "code"};
|#

(define-symbol-macro thread-page-flag 8)
(define-symbol-macro page-type-mask #xF)
(define-symbol-macro single-object-flag 16)

#|
generation_index_t gc_gen_of(lispobj obj, int defaultval) {
    int page = find_page_index((void*)obj);
    if (page >= 0) return page_table[page].gen;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p(obj))
        return immobile_obj_generation(base_pointer(obj));
#endif
    return defaultval;
}
|#

(define-alien-variable text-space-highwatermark (unsigned 64))

(assert (member :little-endian *features*))

(defun addr-widetag (addr)
  ;; assuming little-endian
  (sap-ref-8 (int-sap (logandc2 addr lowtag-mask)) 0))

(define-alien-variable sizetab (* (unsigned 64)))

#+nil
(defun addr-object-size (addr)
  (let ((f (int-sap (deref sizetab (addr-widetag addr)))))
    (if f
        (alien-funcall (sap-alien f (function (unsigned 64) (unsigned 64))) addr)
        cons-size)))

(defun %immobile-object-gen (addr)
  (let* ((offset (if (= (addr-widetag addr) fdefn-widetag) 1 3))
         (gen (sap-ref-8 (int-sap (logandc2 addr lowtag-mask)) offset)))
    (logand (logand gen #x1f) #xf)))

#|
extern sword_t (*sizetab[256])(lispobj *where) ;

typedef sword_t (*sizerfn)(lispobj*);

static inline sword_t object_size(lispobj* where) {
    sizerfn f = sizetab[widetag_of(where)];
    return f ? f(where) : CONS_SIZE;
}
|#

(defun count-immobile-objects (gen results) ; results is an array of length 3
  (declare (ignorable gen))
  (when (member :immobile-space sb-impl:+internal-features+)
    (map-immobile-objects (lambda (obj wt size)
                            (declare (ignore size))
                            (when (eql (%immobile-object-gen (get-lisp-obj-address obj)) gen)
                              (case wt
                                ((#.instance-widetag)  (incf (aref results 0)))
                                ((#.symbol-widetag)    (incf (aref results 1))))))
                          :fixed)
    (map-immobile-objects (lambda (obj wt size)
                            (declare (ignore size))
                            (unless (eql wt filler-widetag)
                              (when (eql (%immobile-object-gen (get-lisp-obj-address obj)) gen)
                                (incf (aref results 2)))))
                          :text))
  (not (every #'zerop results)))

(defun page-type (page)
  (logand (slot (deref page-table page) 'flags)
          page-type-mask))

(defun page-free-p (page)
  (eql free-page-flag (page-type page)))

(defun npage-bytes (pages)
  (* pages gencgc-page-bytes))

(defmacro page-words-used (index)
  `(ash (slot (deref page-table ,index) 'words-used*) -1))

(define-symbol-macro cards-per-page 32)
(define-symbol-macro gencgc-card-shift 10)

(define-alien-variable gc-card-table-mask long)

#+allocation-tracks
;; Set this to 0 to suppress extra track stats.
(define-alien-variable gc-track-report-enabled (signed 32))

(define-symbol-macro scratch-generation
    (1+ +pseudo-static-generation+))

(defmacro page-address (n)
  `(+ dynamic-space-start (* gencgc-page-bytes ,n)))

(defmacro addr-to-card-index (addr)
  `(logand (ash ,addr (- gencgc-card-shift)) gc-card-table-mask))

(defmacro page-to-card-index (n)
  `(addr-to-card-index (page-address ,n)))

(define-alien-variable gc-card-mark (* char))

(define-symbol-macro mark-byte-mask #xff)
(define-symbol-macro card-unmarked #xff)

(defmacro card-dirtyp (index)
  `(not (eql (logand (deref gc-card-mark ,index) mark-byte-mask)
             card-unmarked)))

(defun count-generation-pages (generation)
  "Count the number of pages in the given generation.
  Additionally, return as second value the count of marked pages."
  (declare (type (integer 0 7) generation))
  (let ((total 0)
        (dirty 0))
    (loop
      :for i #|of-type page_index_t|# :from 0 :below next-free-page
      :when (and (not (page-free-p i))
                 (eql (slot (deref page-table i) 'gen) generation))
        :do
           (incf total)
           (loop
             :for card #|of-type long|# :from (page-to-card-index i)
             :for j fixnum :from 0 :below cards-per-page
             :when (card-dirtyp card)
               :do (incf dirty)))
    (values
     total
     ;; divide by cards per page rounding up
     (truncate (+ dirty (1- cards-per-page)) cards-per-page))))

#+nil
(import 'sb-kernel::generation) ; = struct with following slots:
#|
(import 'sb-kernel::bytes-allocated)
(import 'sb-kernel::gc-trigger)
(import 'sb-kernel::bytes-consed-between-gcs)
(import 'sb-kernel::number-of-gcs)
(import 'sb-kernel::number-of-gcs-before-promotion)
(import 'sb-kernel::cum-sum-bytes-allocated)
(import 'sb-kernel::minimum-age-before-gc)
|#

(define-alien-variable generations
    (array sb-kernel::generation #.(1+ +pseudo-static-generation+)))

(define-alien-variable bytes-allocated (unsigned 64))

(defun zero-vector (a)
  (loop
    :for i :below (length a)
    :do (setf (aref a i) 0)))

(defun gc-gen-report-to-stream (stream)
  (format stream "     | Immobile Objects |~%")
  (format stream
          #.(format
             nil "~A~A"
             " Gen layout symbol   code  Boxed   Cons    Raw   Code  SmMix  Mixed  LgRaw LgCode  LgMix"
             " Waste%       Alloc        Trig   Dirty GCs Mem-age~%"))
  (let* ((immobile-matrix (make-array '(8 3)
                                      ;;:element-type '(signed-byte 32)
                                      :initial-element 0))
         (have-immobile-obj 0) ; of-type 'int, used as a bitvector of flags, one per gen
         (immobile-totals (make-array 3
                                      ;;:element-type '(signed-byte 32)
                                      :initial-element 0)))
    ;; loop over generations
    (loop :for gen :from 0 :upto 6
          :for immobile-results := (make-array 3
                                               :displaced-to immobile-matrix
                                               :displaced-index-offset (* 3 gen))
          :when (count-immobile-objects gen immobile-results)
            :do
               (setf (logbitp gen have-immobile-obj) t)
               (incf (aref immobile-totals 0) (aref immobile-matrix gen 0))
               (incf (aref immobile-totals 1) (aref immobile-matrix gen 1))
               (incf (aref immobile-totals 2) (aref immobile-matrix gen 2)))

    (let* (;; Print from the lowest gen that has any allocated pages.
           (gen-begin
            (or (find-if (lambda (gen)
                           (or (logbitp gen have-immobile-obj)
                               (slot (deref generations gen) 'sb-kernel::bytes-allocated)))
                         (range 0 +pseudo-static-generation+))
                (1+ +pseudo-static-generation+)))

           ;; Print up to and including the highest gen that has any allocated pages.
           (gen-end
            (or (find-if (lambda (gen)
                           (plusp (slot (deref generations gen) 'sb-kernel::bytes-allocated)))
                         (range 0 scratch-generation)
                         :from-end t)
                -1))

           ;; page_index_t
           (coltot (make-array 9
                               :element-type 'uword-t
                               :initial-element 0))

           (eden-words-allocated #|of-type 'uword-t|# 0)
           (eden-pages #|of-type 'page-index-t|# 0)

           #+allocation-tracks
           (tr-tot
            (make-array (list +tracks-end+ (+ 9 (1+ gen-end)))
                        :element-type 'uword-t
                        :initial-element 0))
           #+allocation-tracks
           (tr-words-allocated
            (make-array +tracks-end+
                        :element-type 'uword-t
                        :initial-element 0)))
      (declare (ignorable eden-words-allocated eden-pages))

      (loop :with pagect := (make-array 9
                                        :element-type 'uword-t
                                        :initial-element 0)
            :for gen :from gen-begin :upto gen-end
            :for gen-struct := (deref generations gen)
            :for objct := (make-array 3
                                      :displaced-to immobile-matrix
                                      :displaced-index-offset (* 3 gen))
            :do
               (zero-vector pagect)
               (when (eql 0 gen) ;; Count the eden pages
                 (loop
                   :for page #|of-type page_index_t|# :from 0 :below next-free-page
                   :when (and (eql 0 (slot (deref page-table page) 'gen))
                              (not (eql 0 (logand (page-type page) thread-page-flag))))
                     :do
                        (let* ((pt-bits (page-type page))
                               (pt (logandc2 pt-bits thread-page-flag))
                               (column #|of-type int|#
                                (case pt
                                  ((#.page-type-boxed)  0)
                                  ((#.page-type-cons)   1)
                                  ((#.page-type-code)   3)
                                  ((#.page-type-mixed)  5)
                                  (t
                                   (error "Bad eden page subtype: ~X~%" (page-type page))))))
                          ;;(format stream "E p~D flags=~X pt=~X column=~D~%" page pt-bits pt column)
                          (incf (aref pagect column))
                          (incf (aref coltot column))
                          #+allocation-tracks
                          (let ((tr #|of-type 'track-index-t|# (deref page-tracks page)))
                            (incf (aref tr-tot tr column))
                            (incf (aref tr-tot tr (+ 9 gen)))
                            (incf (aref tr-words-allocated tr) (page-words-used page)))
                          (incf eden-pages)
                          (incf eden-words-allocated (page-words-used page))))
                 ;;(format stream "pagect = ~S~%" pagect)(finish-output stream)
                 (let* ((waste #|of-type 'uword-t|#
                         (- (npage-bytes eden-pages) (ash eden-words-allocated word-shift)))
                        (pct-waste #|of-type 'double-float|#
                         (if (plusp eden-pages)
                             (/ (* 100.0d0 (coerce waste 'double-float))
                                (coerce (npage-bytes eden-pages) 'double-float))
                             0.0d0)))
                   (declare (ignorable pct-waste))
                   ;;(format stream "gen=0: waste=~D, pct-waste=~,1F~%" waste pct-waste)
                   #+nil
                   (when eden-pages
                     (format *error-output* "HORKED~%")
                     (format stream
                             "  E ~6D ~6D ~6D ~7D~7D~14D~14D~28,1F ~11X~%"
                             (aref objct 0)
                             (aref objct 1)
                             (aref objct 2)
                             (aref pagect 0)
                             (aref pagect 1)
                             (aref pagect 3)
                             (aref pagect 5)
                             pct-waste
                             (ash eden-words-allocated word-shift))))
                 ;; reset pagect to all zero
                 (zero-vector pagect))
               ;;
               ;; ... still within the loop over generations ...
               ;;
               (let* ((words-allocated #|of-type 'uword-t|# 0)
                      (tot-pages #|of-type 'page_index_t|# 0))
                 (do ((page 0 (1+ page)))
                     ((>= page next-free-page))
                   (when (and (not (page-free-p page))
                              (eql gen (slot (deref page-table page) 'gen))
                              (eql 0 (logand (page-type page) thread-page-flag)))
                     (let ((column #|of-type int|#
                            (case (logand (slot (deref page-table page) 'flags)
                                          (logior single-object-flag page-type-mask))
                              ((#.page-type-boxed)   0)
                              ((#.page-type-cons)    1)
                              ((#.page-type-unboxed) 2)
                              ((#.page-type-code)    3)
                              ((#.page-type-small-mixed) 4)
                              ((#.page-type-mixed)   5)
                              ((#.(logior single-object-flag page-type-unboxed)) 6)
                              ((#.(logior single-object-flag page-type-code))    7)
                              ((#.(logior single-object-flag page-type-mixed))   8)
                              (t
                               (error "Invalid page type #~X (p~d)" (page-type page) page)))))

                       (incf (aref pagect column))
                       (incf (aref coltot column))
                       #+allocation-tracks
                       (let ((tr #|of-type 'track-index-t|# (deref page-tracks page)))
                         #+nil
                         (format stream "gen=~D: p~D tr=~2,'0X column=~D pagect=~D coltot=~D~%"
                                 gen page tr column (aref pagect column) (aref coltot column))
                         (incf (aref tr-tot tr column))
                         (incf (aref tr-tot tr (+ 9 gen)))
                         (incf (aref tr-words-allocated tr) (page-words-used page)))
                       (incf tot-pages)
                       (incf words-allocated (page-words-used page))
                       )))
                 ;;
                 ;; ... still within the loop over generations ...
                 ;;
                 (multiple-value-bind (n-pages n-dirty) #|of-type 'page_index_t|#
                                      (count-generation-pages gen)
                   (declare (ignore n-pages))
                   (let* ((waste #|of-type 'uword-t |#
                           (- (npage-bytes tot-pages)
                              (ash words-allocated word-shift)))
                          (pct-waste #|of-type 'double-float|#
                           (if (plusp tot-pages)
                               (/ (* (coerce waste 'double-float) 100.0d0)
                                  (coerce (npage-bytes tot-pages) 'double-float))
                               0.0d0)))
                     (format stream
                             #.(format nil "~A~{~A~}~A~A~A~A"
                                       "   ~D ~6D ~6D ~6D"
                                       (list
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7)
                                        (page-index-fmt 7))
                                       " ~6,1F " (os-vm-size-fmt 11)
                                       " " (os-vm-size-fmt 11))
                             gen
                             (aref objct 0)
                             (aref objct 1)
                             (aref objct 2)
                             (aref pagect 0)
                             (aref pagect 1)
                             (aref pagect 2)
                             (aref pagect 3)
                             (aref pagect 4)
                             (aref pagect 5)
                             (aref pagect 6)
                             (aref pagect 7)
                             (aref pagect 8)
                             pct-waste
                             (ash words-allocated word-shift)
                             (coerce (slot gen-struct 'sb-kernel::gc-trigger) 'uintptr-t))
                     ;; gen0 pages are never WPed
                     (write-char #\space stream)
                     (if (zerop gen)
                         (format stream "      -")
                         (format stream (page-index-fmt 7) n-dirty))
                     (format stream
                             " ~3d ~7,4F~%"
                             (slot gen-struct 'sb-kernel::number-of-gcs)
                             (generation-average-age gen))
                     ))))
      ;;
      ;; Report totals
      ;;
      (let* ((tot-pages #|of-type 'page-index-t|#
              (+ (aref coltot 0)
                 (aref coltot 1)
                 (aref coltot 2)
                 (aref coltot 3)
                 (aref coltot 4)
                 (aref coltot 5)
                 (aref coltot 6)
                 (aref coltot 7)
                 (aref coltot 8)))
             (waste #|of-type 'uword-t|#
              (- (npage-bytes tot-pages) bytes-allocated))
             (pct-waste #|of-type 'double-float|#
              (/ (* 100.0d0 (coerce waste 'double-float))
                 (coerce (npage-bytes tot-pages) 'double-float)))
             (heap-use-frac #|of-type 'double-float|#
              (/ (* 100.0d0 (coerce bytes-allocated 'double-float))
                 (coerce (dynamic-space-size) 'double-float)))
             (objct #|of-type '(int *)|#
              immobile-totals))
        (format stream
                #.(format nil "~A~{~A~}~A~A~A~A~A"
                          " Tot ~6D ~6D ~6D"
                          (list
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7)
                           (page-index-fmt 7))
                          " ~6,1F" (os-vm-size-fmt 12)
                          " [~,1F% of " (os-vm-size-fmt) " max]~%")
                (aref objct 0)
                (aref objct 1)
                (aref objct 2)
                (aref coltot 0)
                (aref coltot 1)
                (aref coltot 2)
                (aref coltot 3)
                (aref coltot 4)
                (aref coltot 5)
                (aref coltot 6)
                (aref coltot 7)
                (aref coltot 8)
                pct-waste
                (coerce bytes-allocated 'uintptr-t)
                heap-use-frac
                (coerce (dynamic-space-size) 'uintptr-t))
        (gc-tracks-report-to-stream tr-tot tot-pages gen-end stream)))))


(defun gc-tracks-report-to-stream (tr-tot tot-pages gen-end stream)
  "Print the track stats."
  #+allocation-tracks
  (when gc-track-report-enabled
    (format stream
            "~% Track              MiB |  Boxed   Cons    Raw   Code  SmMix  Mixed  LgRaw LgCode  LgMix |")
    (loop :for gen :upto gen-end :do
      (format stream "   Gen~D" gen))
    (terpri stream)
    ;;
    (loop :for tr :below +tracks-end+
          :for tr-tot-pages := (+ (aref tr-tot tr 0)
                                  (aref tr-tot tr 1)
                                  (aref tr-tot tr 2)
                                  (aref tr-tot tr 3)
                                  (aref tr-tot tr 4)
                                  (aref tr-tot tr 5)
                                  (aref tr-tot tr 6)
                                  (aref tr-tot tr 7)
                                  (aref tr-tot tr 8))
          :when (plusp tr-tot-pages)
            :do
               (format stream
                       #.(format nil "~A~{~A~}~A"
                                 "  ~2,'0X  ~5,1F% ~10,1F |"
                                 (list
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7)
                                  (page-index-fmt 7))
                                 " |")
                       tr
                       (/ (* 100.0d0 tr-tot-pages) tot-pages)
                       (/ (*   1.0d0 tr-tot-pages gencgc-page-bytes) (* 1024 1024))
                       (aref tr-tot tr 0)
                       (aref tr-tot tr 1)
                       (aref tr-tot tr 2)
                       (aref tr-tot tr 3)
                       (aref tr-tot tr 4)
                       (aref tr-tot tr 5)
                       (aref tr-tot tr 6)
                       (aref tr-tot tr 7)
                       (aref tr-tot tr 8))
               (loop :for gen :from 0 :upto gen-end :do
                 (format stream (page-index-fmt 7) (aref tr-tot tr (+ 9 gen))))
               (terpri stream))
    (terpri stream)))

#+nil
(defun gc-threads-report-to-stream (stream)
  (format stream
          #.(format nil "~A~A~A"
                    "  Thread  Track  State   TotBytesAlloc"
                    "   "
                    "TLAB:  Boxed         Cons        Mixed       Symbol     SysMixed      SysCons~%")
          )
  (do ((th all-threads (slot th 'next))) ;; th of-type (struct thread *)
      ((not (eql 'NULL th)))
    (when t ;; th->state_word.state == STATE_STOPPED
      (format stream
              "~8D    ~2,'0X     ~2,'0X    ~13D  "
              (slot th 'os-kernel-tid)
              (slot th 'track)
              (slot (slot th 'state-word) 'state) ; value 2 = :stopped
              (+ (ash (coerce (slot th 'tot-bytes-alloc-boxed) '(unsigned 64)) (- n-fixnum-tag-bits))
                 (ash (coerce (slot th 'tot-bytes-alloc-unboxed) '(unsigned 64)) (- n-fixnum-tag-bits))))
      (flet ((_output-tlab (tlab)
               (if (not (slot tlab 'start_addr))
                   (format stream "            -")
                   (let* ((first-page (find-page-index (slot tlab 'start_addr)))
                          (last-page (find-page-index
                                      (coerce (- (coerce (slot tlab 'end_addr) '(unsigned 64))
                                                 n-word-bytes)
                                              'os-vm-address-t)))
                          (n-pages (- (1+ last-page) first-page)))
                     (format stream " ~11Dp" n-pages)))))
        (_output-tlab (slot th 'boxed_tlab))
        (_output-tlab (slot th 'cons_tlab))
        (_output-tlab (slot th 'mixed_tlab))
        (_output-tlab (slot th 'symbol_tlab))
        (_output-tlab (slot th 'sys_mixed_tlab))
        (_output-tlab (slot th 'sys_cons_tlab))
        (terpri stream))
      #+nil
      (format stream "                                        ~12X ~12X ~12X ~12X ~12X ~12X~%"
              (slot (slot th 'boxed_tlab) 'free_pointer)
              (slot (slot th 'cons_tlab) 'free_pointer)
              (slot (slot th 'mixed_tlab) 'free_pointer)
              (slot (slot th 'symbol_tlab) 'free_pointer)
              (slot (slot th 'sys_mixed_tlab) 'free_pointer)
              (slot (slot th 'sys_cons_tlab) 'free_pointer))
      (terpri stream))))
