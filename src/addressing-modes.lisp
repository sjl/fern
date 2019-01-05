(in-package :fern)

(defun-inline operand-at (nes address width)
  (ecase width
    (1 (mref nes address))
    (2 (mref/16 nes address))))

(defun-inline read-operand (nes width)
  (prog1 (operand-at nes (pc nes) width)
    (incf/16 (pc nes) width)))

(defgeneric render-addressing-mode-operand-to-string (mode operand))
(defgeneric render-addressing-mode-operand-to-list (mode operand))

(defmacro define-addressing-mode (mode &key
                                  operand-width
                                  address
                                  (read '(mref nes address))
                                  (write '(mref nes address))
                                  (render-string "~*")
                                  (render-list '`((,mode ,operand)))
                                  (render-key '#'identity))
  "Define an addressing mode.

  `mode` is a symbol representing the mode.  Some metadata will get set in its
  plist.

  `operand-width` is the width of the operand, in bytes.

  `address` is a form to compute the target address.  It will be evaluated in
  a context where `nes` and `operand` are bound.

  `read` is a form to read the value of the target.  It will be evaluated in
  a context where `nes`, `operand`, and `address` are bound.

  `write` is a form to read the value of the target.  It will be evaluated in
  a context where `nes`, `operand`, and `address` are bound.

  The `render-` parameters are used to render operands to disassembly:

  * `render-key` will be called on the operand prior to rendering.
  * `render-string` is a string that will be used in `(format nil render-string
    operand)` to render the operand to a string of disassembly.
  * `render-list` is form that will be used to render the operand to a list of
    disassembly.

  "
  `(progn
     (setf (get ',mode 'operand-width) ,operand-width
           (get ',mode 'operand) ',(if (zerop operand-width)
                                     nil
                                     `(read-operand nes ,operand-width))
           (get ',mode 'address) ',address
           (get ',mode 'read) ',read
           (get ',mode 'write) ',write)
     (defmethod render-addressing-mode-operand-to-string ((mode (eql ',mode)) operand)
       (format nil ,render-string (funcall ,render-key operand)))
     (defmethod render-addressing-mode-operand-to-list ((mode (eql ',mode)) operand)
       (let ((operand (funcall ,render-key operand)))
         (declare (ignorable operand))
         ,(or render-list '`(,mode ,operand))))
     ',mode))


(defun buggy-mref/16 (nes address)
  "Read a 16-bit value from `nes` at `address`, accounting for the notorious bug.

  The 6502 has a well-known bug where trying to read an operand that crosses
  a page boundary doesn't work as expected.  When trying to read an operand at
  xxFF, instead of reading the LSB from xxFF and the MSB from yy00, the MSB gets
  read from xx00 instead.

  "
  (if (= #xFF (lsb address))
    (cat (mref nes address)
         (mref nes (logand address #xFF00)))
    (mref/16 nes address)))

(define-addressing-mode implied
  :operand-width 0
  :address nil
  :read nil
  :write nil
  :render-list (list))

(define-addressing-mode accumulator
  :operand-width 0
  :address nil
  :read a
  :write a
  :render-string "A~*"
  :render-list '(accumulator))

(define-addressing-mode immediate
  :operand-width 1
  :address nil
  :read operand
  :write nil
  :render-string "#~2,'0X")

(define-addressing-mode zero-page
  :operand-width 1
  :address operand
  :render-string "$~2,'0X")

(define-addressing-mode zero-page-x
  :operand-width 1
  :address (+/8 operand x)
  :render-string "$~2,'0X,X")

(define-addressing-mode zero-page-y
  :operand-width 1
  :address (+/8 operand y)
  :render-string "$~2,'0X,Y")

(define-addressing-mode relative
  :operand-width 1
  :address (+/16 pc (signed/8 operand))
  :read nil
  :write nil
  :render-string "~@D"
  :render-key #'signed/8)

(define-addressing-mode absolute
  :operand-width 2
  :address operand
  :render-string "$~4,'0X")

(define-addressing-mode absolute-x
  :operand-width 2
  :address (+/16 operand x)
  :render-string "$~4,'0X,X")

(define-addressing-mode absolute-y
  :operand-width 2
  :address (+/16 operand y)
  :render-string "$~4,'0X,Y")

(define-addressing-mode indirect
  :operand-width 2
  :address (buggy-mref/16 nes operand)
  :read nil
  :write nil
  :render-string "($~4,'0X)")

(define-addressing-mode pre-indexed
  :operand-width 1
  :address (buggy-mref/16 nes (+/8 operand x))
  :render-string "($~2,'0X,X)")

(define-addressing-mode post-indexed
  :operand-width 1
  :address (+/16 (buggy-mref/16 nes operand) y)
  :render-string "($~2,'0X),Y")


(defmacro with-addressing-mode (mode &body body)
  "Evaluate `body` with the appropriate addressing mode context.

  This macro will bind several things to the appropriate values based on the
  addressing mode:

  * `operand` is the raw operand from the instruction.
  * `address` is the address of the target.
  * `(value)` will read the value of the target.
  * `(setf (value) …)` will set the value of the target.

  Not all addressing modes will support all of these.  For example:

  * `immediate` mode will not bind an address, and cannot set the value.
  * `accumulator` mode will not bind an address.
  * `relative` mode only binds the address, and cannot read or set the value.
  * `implied* mode won't set anything at all.

  "
  (let ((operand (get mode 'operand))
        (address (get mode 'address))
        (read (get mode 'read))
        (write (get mode 'write)))
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

