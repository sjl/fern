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

