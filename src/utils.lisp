(in-package :fern)

;;;; Types --------------------------------------------------------------------
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype s8 () '(signed-byte 8))
(deftype function-designator () '(or function symbol))


;;;; Bitwise Operations and Modular Arithmetic --------------------------------
(defun-inline wrap (value width)
  (ldb (byte width 0) value))

(defun-inline bits (integer start end)
  (ldb (byte (- (1+ end) start) start) integer))

(defun-inline cat (low high &optional (width 8))
  "Concatenate two `width`-bit bytes."
  (logior low (ash high width)))

(defun-inline signed (unsigned-value width)
  (if (logbitp (1- width) unsigned-value)
    (dpb unsigned-value (byte (1- width) 0) -1)
    unsigned-value))

(defun-inline signed/8 (unsigned-value)
  (signed unsigned-value 8))

(defun-inline signed/16 (unsigned-value)
  (signed unsigned-value 16))


(defun msb (integer) (ldb (byte 8 8) integer))
(defun lsb (integer) (ldb (byte 8 0) integer))

(defun-inline +/8 (&rest args) (wrap (apply #'+ args) 8))
(defun-inline -/8 (&rest args) (wrap (apply #'- args) 8))
(defun-inline +/16 (&rest args) (wrap (apply #'+ args) 16))
(defun-inline -/16 (&rest args) (wrap (apply #'- args) 16))

(defun-inline 1+/8 (x) (+/8 x 1))
(defun-inline 1-/8 (x) (-/8 x 1))
(defun-inline 1+/16 (x) (+/16 x 1))
(defun-inline 1-/16 (x) (-/16 x 1))

(define-modify-macro incf/8 (&optional (delta 1)) +/8)
(define-modify-macro decf/8 (&optional (delta 1)) -/8)
(define-modify-macro incf/16 (&optional (delta 1)) +/16)
(define-modify-macro decf/16 (&optional (delta 1)) -/16)

(defun-inline ldbit (position integer)
  (ldb (byte 1 position) integer))

(defun-inline dpbit (new-bit position integer)
  (dpb new-bit (byte 1 position) integer))


;;;; Memory Arrays ------------------------------------------------------------
(deftype memory (&optional (size '*))
  `(simple-array u8 (,size)))

(defun make-memory (size)
  (make-array size :initial-element 0 :element-type 'u8))


(defun mirrored-address (start end address &key from-zero)
  "Return the base address in `start` to `end` after mirroring.

  The NES has several regions of memory that are mirrored, e.g.:

    total  2000 +--------------+        4000 +---------------+
                | Mirror of    |             | Mirror of     |
                | 0000 - 0800  |             | 2000 - 2008   |
    end    0800 +--------------+        2008 +---------------+
                | Internal RAM |             | I/O Registers |
    start  0000 +--------------+        2000 +---------------+

  This function will take an address between `start` and `total` (not checked)
  and return the appropriate address between `start` and `end` to use.

  If `from-zero` is given, the start offset will be subtracted from the result.
  This is handy for indexing into arrays.

  "
  (+ (if from-zero 0 start) (mod address (- end start))))


(defun aref% (array index)
  (aref array (mod index (array-total-size array))))

(defun (setf aref%) (new-value array index)
  (setf (aref array (mod index (array-total-size array)))
        new-value))


(defmacro address-case ((index-symbol address) &body clauses)
  "Evaluate the appropriate clause from `clauses` for the given `address`.

  This macro is useful when you have a series of contiguous chunks of memory in
  the address space and want to easily index into separate arrays in the Lisp
  world.

  Each clause must be of the form:

    (upper-bound . body)

  where `upper-bound` is a constant integer or `t`.

  Using this macro looks like this:

    (address-case (index some-address)
      (#x4000 (aref foo index))  ;; array foo contains memory at 0000-3FFF
      (#x8000 (aref bar index))  ;; array bar contains memory at 4000-7FFF
      (#x10000 (aref baz index)) ;; array baz contains memory at 8000-FFFF
      (t (error \"Address out of range.\")))

  In this example, `some-address` will be successively compared against each
  clause with `(< some-address upper-bound)`.  When a clause matches,
  `index-symbol` will be bound to `(- address previous-upper-bound)` and the
  clause's body evaluated.

  "
  (once-only (address)
    `(cond
       ,@(iterate
          (for (value . body) :in clauses)
          (for prev :previous value :initially 0)
          (check-type value (or (eql t) integer))
          (collect
            (if (eq t value)
              `(t ,@body)
              `((< ,address ,value) (let ((,index-symbol (- ,address ,prev)))
                                      (declare (ignorable ,index-symbol))
                                      ,@body))))))))


;;;; Streams ------------------------------------------------------------------
(defun read-memory-from-stream (stream length &optional eof-message)
  "Read a chunk of `length` bytes from `stream` into a fresh memory array.

  `stream` must be a binary input stream.

  If `eof-message` is given an error will be signaled if end of file is
  encountered before all the bytes have been read.

  "
  (let* ((result (make-memory length))
         (read-length (read-sequence result stream)))
    (when eof-message
      (assert (= length read-length) () eof-message))
    result))

(defun assert-eof (stream message)
  "Signal an error with text `message` if `stream` is not at EOF."
  (assert (eql :eof (read-byte stream nil :eof)) () message))

(defun read-string-from-stream (stream length)
  "Read and return an ASCII string of length `length` from `stream`.

  `stream` must be a binary input stream.

  "
  (iterate (repeat length)
           (for byte = (read-byte stream))
           (collect (code-char byte) :result-type string)))

;;;; Misc ---------------------------------------------------------------------
(defun-inline todo ()
  (error "TODO"))

(defun required (name)
  (error "Required field: ~A" name))

(defun kb (n)
  (* 1024 n))

(defun unimplemented (&rest args)
  (declare (ignore args))
  (error "Unimplemented"))


