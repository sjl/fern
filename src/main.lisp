(in-package :fern)

;;;; Utils --------------------------------------------------------------------
(deftype memory (size)
  `(simple-array u8 (,size)))

(defun make-memory (size)
  (make-array size :initial-element 0 :element-type 'u8))

(defun unimplemented (&rest args)
  (declare (ignore args))
  (error "Unimplemented cartridge reader/writer."))


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
  (funcall (cartridge-reader (cartridge nes)) address))

(defun cartridge-write (nes address value)
  (funcall (cartridge-writer (cartridge nes)) address value)
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


(defun-inline stack-address (nes)
  (logior #x0100 (sp nes)))

(defun stack-push (nes value)
  (setf (mref nes (stack-address nes)) value)
  (decf/8 (sp nes)))

(defun stack-push/16 (nes value)
  (stack-push nes (msb value))
  (stack-push nes (lsb value)))

(defun stack-peek (nes)
  (mref nes (stack-address nes)))

(defun stack-peek/16 (nes)
  (mref/16 nes (stack-address nes)))

(defun stack-pop (nes)
  (prog1 (stack-peek nes)
    (incf/8 (sp nes))))

(defun stack-pop/16 (nes)
  (prog1 (stack-peek/16 nes)
    (incf/8 (sp nes) 2)))


;;;; Addressing Modes ---------------------------------------------------------
(defun-inline read-operand (nes width)
  (with-nes (nes)
    (ecase width
      (8 (prog1 (mref nes pc)
           (incf pc)))
      (16 (prog1 (cat (mref nes pc)
                      (mref nes (1+/16 pc)))
            (incf pc 2))))))

(defgeneric addressing-mode-operand (mode))
(defgeneric addressing-mode-address (mode))
(defgeneric addressing-mode-read (mode))
(defgeneric addressing-mode-write (mode))


(defmacro define-addressing-mode (mode &key
                                  operand-width
                                  address
                                  (read '(mref nes address))
                                  (write '(mref nes address)))
  `(progn
     (defmethod addressing-mode-operand ((mode (eql ',mode)))
       ',(if (null operand-width)
           nil
           `(read-operand nes ,operand-width)))
     (defmethod addressing-mode-address ((mode (eql ',mode))) ',address)
     (defmethod addressing-mode-read ((mode (eql ',mode))) ',read)
     (defmethod addressing-mode-write ((mode (eql ',mode))) ',write)
     ',mode))

(define-addressing-mode implied
  :operand-width nil
  :address nil
  :read nil
  :write nil)

(define-addressing-mode accumulator
  :operand-width nil
  :address nil
  :read a
  :write a)

(define-addressing-mode immediate
  :operand-width 8
  :address nil
  :read operand
  :write nil)

(define-addressing-mode zero-page
  :operand-width 8
  :address operand)

(define-addressing-mode zero-page-x
  :operand-width 8
  :address (+/8 operand x))

(define-addressing-mode zero-page-y
  :operand-width 8
  :address (+/8 operand y))

(define-addressing-mode relative
  :operand-width 8
  :address (+/16 pc (signed operand 8))
  :read nil
  :write nil)

(define-addressing-mode absolute
  :operand-width 16
  :address operand)

(define-addressing-mode absolute-x
  :operand-width 16
  :address (+/16 operand x))

(define-addressing-mode absolute-y
  :operand-width 16
  :address (+/16 operand y))

(define-addressing-mode indirect
  :operand-width 16
  :address (if (= #xFF (lsb operand)) ;; handle this notorious 6502 bug
             (cat (mref nes operand)
                  (mref nes (logand operand #xFF00)))
             (mref/16 nes operand))
  :read nil
  :write nil)

(define-addressing-mode pre-indexed
  :operand-width 8
  :address (mref/16 nes (+/8 operand x)))

(define-addressing-mode post-indexed
  :operand-width 8
  :address (+/16 (mref/16 nes operand) y))


(defmacro with-addressing-mode (mode &body body)
  (let ((operand (addressing-mode-operand mode))
        (address (addressing-mode-address mode))
        (read (addressing-mode-read mode))
        (write (addressing-mode-write mode)))
    `(let* (,@(when operand `((operand ,operand)))
            ,@(when address `((address ,address))))
       (declare ,@(when operand '((ignorable operand)))
                ,@(when address '((ignorable address))))
       (flet (,@(when read
                  `((value () ,read)))
              ,@(when write
                  `(((setf value) (new-value) (setf ,write new-value)))))
         (declare ,@(when read '((ignorable (function value))))
                  ,@(when write '((ignorable (function (setf value))))))
         ,@body))))


;;;; Opcodes ------------------------------------------------------------------
(defun illegal-opcode (opcode)
  (error "Illegal opcode: ~2,'0X" opcode))

(eval-dammit (defparameter *debug-opcodes* t))

(defparameter *opcode-functions*
  (iterate (for opcode :from 0 :below 256)
           (collect (curry #'illegal-opcode opcode) :result-type vector)))

(defparameter *opcode-data*
  (iterate
    (for opcode :from 0 :below 256)
    (collect `(:opcode ,opcode :doc "Illegal opcode" :legal nil :cycles nil)
             :result-type vector)))


(defmacro define-opcode% (opcode cycles name documentation &body body)
  `(progn
     (declaim (ftype (function (nes) null) ,name))
     (defun ,name (nes)
       ,documentation
       (with-nes (nes)
         ,@body
         (incf cycles ,cycles))
       nil)
     (setf (aref *opcode-functions* ,opcode)
           ,(if *debug-opcodes* `',name `#',name)
           (aref *opcode-data* ,opcode)
           `(:opcode ,,opcode :doc ,,documentation :legal t :cycles ,,cycles))
     ',name))

(defmacro define-opcode (name addressing-mode-map documentation &body body)
  `(progn
     ,@(iterate (for (opcode addressing-mode cycles) :in addressing-mode-map)
                (for full-name = (symb name '/ addressing-mode))
                (collect
                  `(define-opcode% ,opcode ,cycles ,full-name ,documentation
                     (with-addressing-mode ,addressing-mode ,@body))))))


(defmacro copy-and-set-flags (source destination &rest flags)
  (with-gensyms (val)
    `(let ((,val ,source))
       (setf ,destination ,val
             ,@(iterate
                 (for flag :in flags)
                 (appending (ecase flag
                              (zero `(zero (zerop ,val)))
                              (negative `(negative (logbitp 7 ,val))))))))))


;;;; Load and Store -----------------------------------------------------------
(define-opcode lda
    ((#xA9 immediate 2)
     (#xA5 zero-page 3)
     (#xB5 zero-page-x 4)
     (#xAD absolute 4)
     (#xBD absolute-x 4)
     (#xB9 absolute-y 4)
     (#xA1 pre-indexed 6)
     (#xB1 post-indexed 5))
  "Load accumulator"
  (copy-and-set-flags (value) a zero negative))

(define-opcode ldx
    ((#xA2 immediate 2)
     (#xA6 zero-page 3)
     (#xB6 zero-page-y 4)
     (#xAE absolute 4)
     (#xBE absolute-y 4))
  "Load X register"
  (copy-and-set-flags (value) x zero negative))

(define-opcode ldy
    ((#xA0 immediate 2)
     (#xA4 zero-page 3)
     (#xB4 zero-page-x 4)
     (#xAC absolute 4)
     (#xBC absolute-x 4))
  "Load Y register"
  (copy-and-set-flags (value) y zero negative))

(define-opcode sta
    ((#x85 zero-page 3)
     (#x95 zero-page-x 4)
     (#x8D absolute 4)
     (#x9D absolute-x 5)
     (#x99 absolute-y 5)
     (#x81 pre-indexed 6)
     (#x91 post-indexed 6))
  "Store accumulator"
  (setf (value) a))

(define-opcode stx
    ((#x86 zero-page 3)
     (#x96 zero-page-y 4)
     (#x8E absolute 4))
  "Store X register"
  (setf (value) x))

(define-opcode sty
    ((#x84 zero-page 3)
     (#x94 zero-page-y 4)
     (#x8C absolute 4))
  "Store Y register"
  (setf (value) y))


;;;; Register Transfers -------------------------------------------------------
(define-opcode tax
    ((#xAA implied 2))
  "Transfer accumulator to X"
  (copy-and-set-flags a x zero negative))

(define-opcode tay
    ((#xA8 implied 2))
  "Transfer accumulator to Y"
  (copy-and-set-flags a y zero negative))

(define-opcode txa
    ((#x8A implied 2))
  "Transfer X to accumulator"
  (copy-and-set-flags x a zero negative))

(define-opcode tya
    ((#x98 implied 2))
  "Transfer Y to accumulator"
  (copy-and-set-flags y a zero negative))


;;;; Stack Operations ---------------------------------------------------------
(define-opcode tsx
    ((#xBA implied 2))
  "Transfer stack pointer to X"
  (copy-and-set-flags sp x zero negative))

(define-opcode txs
    ((#x9A implied 2))
  "Transfer X to stack pointer"
  (setf sp x))

(define-opcode pha
    ((#x48 implied 3))
  "Push accumulator onto the stack"
  (stack-push nes a))

(define-opcode php
    ((#x08 implied 3))
  "Push processor status onto the stack"
  (stack-push nes status))

(define-opcode pla
    ((#x68 implied 4))
  "Pull accumulator from the stack"
  (copy-and-set-flags (stack-pop nes) a zero negative))

(define-opcode plp
    ((#x28 implied 4))
  "Pull processor status from the stack"
  (setf status (stack-pop nes)))


;;;; Logical Operations -------------------------------------------------------
(define-opcode and
    ((#x29 immediate 2)
     (#x25 zero-page 3)
     (#x35 zero-page-x 4)
     (#x2D absolute 4)
     (#x3D absolute-x 4)
     (#x39 absolute-y 4)
     (#x21 pre-indexed 6)
     (#x31 post-indexed 5))
  "Logical AND"
  (copy-and-set-flags (logand a (value)) a zero negative))

(define-opcode eor
    ((#x49 immediate 2)
     (#x45 zero-page 3)
     (#x55 zero-page-x 4)
     (#x4D absolute 4)
     (#x5D absolute-x 4)
     (#x59 absolute-y 4)
     (#x41 pre-indexed 6)
     (#x51 post-indexed 5))
  "Logical XOR"
  (copy-and-set-flags (logxor a (value)) a zero negative))

(define-opcode ora
    ((#x09 immediate 2)
     (#x05 zero-page 3)
     (#x15 zero-page-x 4)
     (#x0D absolute 4)
     (#x1D absolute-x 4)
     (#x19 absolute-y 4)
     (#x01 pre-indexed 6)
     (#x11 post-indexed 5))
  "Logical IOR"
  (copy-and-set-flags (logior a (value)) a zero negative))

(define-opcode bit
    ((#x24 zero-page 3)
     (#x2C absolute 4))
  "Bit test"
  (let ((result (logand a (value))))
    (setf zero (zerop result)
          overflow (logbitp 6 result)
          negative (logbitp 7 result))))


;;;; Arithmetic Operations ----------------------------------------------------
(define-opcode adc
    ((#x69 immediate 2)
     (#x65 zero-page 3)
     (#x75 zero-page-x 4)
     (#x6D absolute 4)
     (#x7D absolute-x 4)
     (#x79 absolute-y 4)
     (#x61 pre-indexed 6)
     (#x71 post-indexed 5))
  "Add with Carry"
  (let* ((full (+ a (value) carry-bit))
         (result (wrap full 8)))
    (copy-and-set-flags result a zero negative)
    (setf carry (> full #xFF)
          overflow (TODO))))

(define-opcode sbc
    ((#xE9 immediate 2)
     (#xE5 zero-page 3)
     (#xF5 zero-page-x 4)
     (#xED absolute 4)
     (#xFD absolute-x 4)
     (#xF9 absolute-y 4)
     (#xE1 pre-indexed 6)
     (#xF1 post-indexed 5))
  "Subtract with carry"
  (let* ((full (- a (value) (if carry 0 1)))
         (result (wrap full 8)))
    (copy-and-set-flags result a zero negative)
    (setf carry (minusp full)
          overflow (TODO))))

(define-opcode cmp
    ((#xC9 immediate 2)
     (#xC5 zero-page 3)
     (#xD5 zero-page-x 4)
     (#xCD absolute 4)
     (#xDD absolute-x 4)
     (#xD9 absolute-y 4)
     (#xC1 pre-indexed 6)
     (#xD1 post-indexed 5))
  "Compare accumulator"
  (let ((result (- a (value))))
    (setf carry (>= result 0)
          zero (zerop result)
          negative (logbitp 7 result))))

(define-opcode cpx
    ((#xE0 immediate 2)
     (#xE4 zero-page 3)
     (#xEC absolute 4))
  "Compare X register"
  (let ((result (- x (value))))
    (setf carry (>= result 0)
          zero (zerop result)
          negative (logbitp 7 result))))

(define-opcode cpy
    ((#xC0 immediate 2)
     (#xC4 zero-page 3)
     (#xCC absolute 4))
  "Compare Y register"
  (let ((result (- y (value))))
    (setf carry (>= result 0)
          zero (zerop result)
          negative (logbitp 7 result))))


;;;; Increment and Decrement --------------------------------------------------
(define-opcode inc
    ((#xE6 zero-page 5)
     (#xF6 zero-page-x 6)
     (#xEE absolute 6)
     (#xFE absolute-x 7))
  "Increment"
  (copy-and-set-flags (1+/8 (value)) (value) zero negative))

(define-opcode inx
    ((#xE8 implied 2))
  "Increment X register"
  (copy-and-set-flags (1+/8 x) x zero negative))

(define-opcode iny
    ((#xC8 implied 2))
  "Increment Y register"
  (copy-and-set-flags (1+/8 y) y zero negative))

(define-opcode dec
    ((#xC6 zero-page 5)
     (#xD6 zero-page-x 6)
     (#xCE absolute 6)
     (#xDE absolute-x 7))
  "Decrement"
  (copy-and-set-flags (1-/8 (value)) (value) zero negative))

(define-opcode dex
    ((#xCA implied 2))
  "Decrement X register"
  (copy-and-set-flags (1-/8 x) x zero negative))

(define-opcode dey
    ((#x88 implied 2))
  "Decrement Y register"
  (copy-and-set-flags (1-/8 y) y zero negative))


;;;; Shift Operations ---------------------------------------------------------
(define-opcode asl
    ((#x0A accumulator 2)
     (#x06 zero-page 5)
     (#x16 zero-page-x 6)
     (#x0E absolute 6)
     (#x1E absolute-x 7))
  "Arithmetic shift left"
  (let* ((full (ash (value) 1))
         (result (wrap full 8)))
    (copy-and-set-flags result (value) zero negative)
    (setf carry (logbitp 8 full))))

(define-opcode lsr
    ((#x4A accumulator 2)
     (#x46 zero-page 5)
     (#x56 zero-page-x 6)
     (#x4E absolute 6)
     (#x5E absolute-x 7))
  "Logical shift right"
  (let* ((val (value))
         (result (ash val -1)))
    (copy-and-set-flags result (value) zero negative)
    (setf carry (logbitp 0 val))))

(define-opcode rol
    ((#x2A accumulator 2)
     (#x26 zero-page 5)
     (#x36 zero-page-x 6)
     (#x2E absolute 6)
     (#x3E absolute-x 7))
  "Rotate left"
  (let* ((full (-<> (value)
                 (ash <> 1)
                 (dpbit carry-bit 0 <>)))
         (result (wrap full 8)))
    (copy-and-set-flags result (value) zero negative)
    (setf carry (logbitp 8 full))))

(define-opcode ror
    ((#x6A accumulator 2)
     (#x66 zero-page 5)
     (#x76 zero-page-x 6)
     (#x6E absolute 6)
     (#x7E absolute-x 7))
  "Rotate right"
  (let* ((val (value))
         (result (-<> val
                   (ash <> -1)
                   (dpbit carry-bit 7 <>))))
    (copy-and-set-flags result (value) zero negative)
    (setf carry (logbitp 0 val))))


;;;; Jumps and Calls ----------------------------------------------------------
(define-opcode jmp
    ((#x4C absolute 3)
     (#x6C indirect 5))
  "Jump to location"
  (setf pc address))

(define-opcode jsr
    ((#x20 absolute 6))
  "Jump to subroutine"
  (stack-push/16 nes (1-/16 pc))
  (setf pc address))

(define-opcode rts
    ((#x60 implied 6))
  "Return from subroutine"
  (setf pc (1+/16 (stack-pop/16 nes))))


;;;; Branching ----------------------------------------------------------------
(defun page-crossed-p (a b)
  (/= (msb a) (msb b)))

(defun branch (nes destination)
  (with-nes (nes)
    (incf cycles (if (page-crossed-p pc destination) 2 1))
    (setf pc destination)))


(define-opcode bcs
    ((#xB0 relative 2))
  "Branch if carry set"
  (when carry (branch nes address)))

(define-opcode bcc
    ((#x90 relative 2))
  "Branch if carry clear"
  (unless carry (branch nes address)))

(define-opcode beq
    ((#xF0 relative 2))
  "Branch if zero set"
  (when zero (branch nes address)))

(define-opcode bne
    ((#xD0 relative 2))
  "Branch if zero clear"
  (unless zero (branch nes address)))

(define-opcode bmi
    ((#x30 relative 2))
  "Branch if negative set"
  (when negative (branch nes address)))

(define-opcode bpl
    ((#x10 relative 2))
  "Branch if negative clear"
  (unless negative (branch nes address)))

(define-opcode bvs
    ((#x70 relative 2))
  "Branch if overflow set"
  (when overflow (branch nes address)))

(define-opcode bvc
    ((#x50 relative 2))
  "Branch if overflow clear"
  (unless overflow (branch nes address)))


;;;; Status Flag Operations ---------------------------------------------------
(define-opcode clc
    ((#x18 implied 2))
  "Clear carry"
  (setf carry nil))

(define-opcode cld
    ((#xD8 implied 2))
  "Clear decimal mode"
  (setf decimal-mode nil))

(define-opcode cli
    ((#x58 implied 2))
  "Clear interrupt disable"
  (setf interrupt-disable nil))

(define-opcode clv
    ((#xB8 implied 2))
  "Clear overflow"
  (setf overflow nil))

(define-opcode sec
    ((#x38 implied 2))
  "Set carry"
  (setf carry t))

(define-opcode sed
    ((#xF8 implied 2))
  "Set decimal mode"
  (setf decimal-mode t))

(define-opcode sei
    ((#x78 implied 2))
  "Set interrupt disable"
  (setf interrupt-disable t))


;;;; System Functions ---------------------------------------------------------
(define-opcode brk
    ((#x00 implied 7))
  "Force an interrupt"
  (setf break-command t)
  (stack-push/16 nes pc)
  (stack-push nes status)
  (setf pc (mref/16 nes #xFFFE)))

(define-opcode nop
    ((#xEA implied 2))
  "No operation")

(define-opcode rti
    ((#x40 implied 6))
  "Return from interrupt"
  (setf status (stack-pop nes)
        pc (stack-pop/16 nes)))


