(in-package :fern)

;;;; NES ----------------------------------------------------------------------
(defstruct (nes (:conc-name nil))
  (running t :type boolean)
  (a 0 :type u8)
  (x 0 :type u8)
  (y 0 :type u8)
  (pc 0 :type u16)
  (sp 0 :type u8)
  (status 0 :type u8)
  (ram (make-memory #x0800) :type (memory #x0800))
  (cartridge (make-cartridge nil nil nil nil) :type cartridge)
  (ppu nil :type (or null ppu))
  (apu nil :type (or null apu))
  (cycles 0 :type fixnum))

(defmacro define-flag (bit name)
  "Define a flag called `name` for bit `bit` of the processor status register.

   Defining a flag called `name` will define four functions:

  * `(name)`: return the flag as a boolean (t = 1, nil = 0).
  * `(name-bit)`: return the flag as a bit.
  * `(setf name)`: set the flag as a boolean (t = 1, nil = 0).
  * `(setf name-bit)`: set the flag to a bit.

  "
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
             new-value))
     ',name))

(define-flag 0 carry)
(define-flag 1 zero)
(define-flag 2 interrupt-disable)
(define-flag 3 decimal-mode)
(define-flag 4 break-command)
(define-flag 6 overflow)
(define-flag 7 negative)

(define-with-macro (nes :conc-name nil)
  ;; this magic thing saves a LOT of typing later
  running
  a x y status
  pc sp
  ram
  cartridge
  cycles
  carry zero interrupt-disable decimal-mode break-command overflow negative
  carry-bit zero-bit interrupt-disable-bit
  decimal-mode-bit break-command-bit
  overflow-bit negative-bit)


;;;; Vectors ------------------------------------------------------------------
;;; A "vector" in NES terminology is just a two-byte address near the end of
;;; memory.  They're used to tell the NES where to go after resetting,
;;; interrupting, etc.

(defun nmi-vector (nes)   (mref/16 nes #xFFFA))
(defun reset-vector (nes) (mref/16 nes #xFFFC))
(defun irq-vector (nes)   (mref/16 nes #xFFFE))


;;;; Reset --------------------------------------------------------------------
(defun reset (nes)
  "Reset `nes`."
  (with-nes (nes)
    ;; https://wiki.nesdev.com/w/index.php/CPU_power_up_state
    (setf status #x34
          a 0
          x 0
          y 0
          sp #xFD
          pc (reset-vector nes))))

