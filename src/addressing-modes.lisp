(in-package :fern)

(defun-inline operand-at (nes address width)
  (ecase width
    (1 (mref nes address))
    (2 (cat (mref nes address)
            (mref nes (1+/16 address))))))

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
  ;; handle the notorious 6502 bug
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
  ;; :render-string "#~2,'0X"
  :render-string "#$~2,'0X") ; fuckin nestest

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
  ;; :render-string "~@D"
  ;; :render-key #'signed/8
  :render-string "$~4,'0X" ; fucking nestest
  :render-key (lambda (operand) (+/16 (signed/8 operand) 2 (pc *current*))))

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

