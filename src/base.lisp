(in-package :fern)

(defparameter *current* nil)

;;;; Cartridges (aka Mappers) -------------------------------------------------
(defgeneric make-cartridge (id prg chr ram))

(defstruct (cartridge (:constructor nil))
  (id (required 'id) :type (or null (integer 0 256)))
  (reader #'unimplemented :type function-designator)
  (writer #'unimplemented :type function-designator)
  (prg)
  (chr)
  (wram))


;;;; NES ----------------------------------------------------------------------
(defstruct (nes (:conc-name nil))
  (running t :type boolean)
  (a 0 :type u8)
  (x 0 :type u8)
  (y 0 :type u8)
  (status 0 :type u8)
  (pc 0 :type u16)
  (sp #xFF :type u8)
  (ram (make-memory #x0800) :type (memory #x0800))
  (cartridge (make-cartridge nil nil nil nil) :type cartridge)
  (cycles 0 :type fixnum))

(defmacro define-flag (bit name)
  `(progn
     (defun ,name (nes)
       (logbitp ,bit (status nes)))
     (defun ,(symb name '-bit) (nes)
       (ldbit ,bit (status nes)))
     (defun (setf ,name) (new-value nes)
       (setf (ldb (byte 1 ,bit) (status nes))
             (if new-value 1 0))
       new-value)
     (defun (setf ,(symb name '-bit)) (new-value nes)
       (setf (ldb (byte 1 ,bit) (status nes))
             new-value))))

(define-flag 0 carry)
(define-flag 1 zero)
(define-flag 2 interrupt-disable)
(define-flag 3 decimal-mode)
(define-flag 4 break-command)
(define-flag 6 overflow)
(define-flag 7 negative)


(define-with-macro (nes :conc-name nil)
  running
  a x y status
  pc sp
  ram
  cartridge
  cycles
  carry zero interrupt-disable decimal-mode break-command overflow negative
  carry-bit zero-bit interrupt-disable-bit decimal-mode-bit break-command-bit overflow-bit negative-bit)



(defun nmi-vector (nes) (mref/16 nes #xFFFA))
(defun reset-vector (nes) (mref/16 nes #xFFFC))
(defun irq-vector (nes) (mref/16 nes #xFFFE))

(defun reset (nes)
  (setf *current* nes)
  (with-nes (nes)
    ;; https://wiki.nesdev.com/w/index.php/CPU_power_up_state
    (setf status #x24 ; todo fix this to be #x34 once we're done with nestest
          a 0
          x 0
          y 0
          sp #xFD
          pc (reset-vector nes))))


;;;; Memory -------------------------------------------------------------------
(defun internal-read (nes address)
  (if (< address #x2000)
    (aref (ram nes) (mod address #x800))
    (TODO)))

(defun internal-write (nes address value)
  (if (< address #x2000)
    (setf (aref (ram nes) (mod address #x800)) value)
    (TODO))
  nil)


(defun cartridge-read (nes address)
  (let ((cartridge (cartridge nes)))
    (funcall (cartridge-reader cartridge) cartridge address)))

(defun cartridge-write (nes address value)
  (let ((cartridge (cartridge nes)))
    (funcall (cartridge-writer cartridge) cartridge address value))
  nil)


(defun mref (nes address)
  (if (< address #x4020)
    (internal-read nes address)
    (cartridge-read nes address)))

(defun (setf mref) (new-value nes address)
  (if (< address #x4020)
    (internal-write nes address new-value)
    (cartridge-write nes address new-value))
  new-value)


(defun mref/16 (nes address)
  (cat (mref nes address)
       (mref nes (1+/16 address))))


;;;; Stack --------------------------------------------------------------------
(defun-inline stack-address (nes)
  (logior #x0100 (sp nes)))


(defun stack-push (nes value)
  (setf (mref nes (stack-address nes)) value)
  (decf/8 (sp nes)))

(defun stack-push/16 (nes value)
  (stack-push nes (msb value))
  (stack-push nes (lsb value)))


(defun stack-pop (nes)
  (incf/8 (sp nes))
  (mref nes (stack-address nes)))

(defun stack-pop/16 (nes)
  (cat (stack-pop nes)
       (stack-pop nes)))

