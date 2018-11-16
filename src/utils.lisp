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

(defun-inline cat-bytes (width low high)
  (logior low (ash high width)))


;;;; Misc ---------------------------------------------------------------------
(defun todo ()
  (error "TODO"))

(defun kb (n)
  (* 1024 n))

