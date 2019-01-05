(in-package :fern)

(defun disassemble-op (nes address)
  "Disassemble the operation at `address`.

  Returns three values:

  * The disassembly as a list.
  * The disassembly as a string.
  * A list of the raw bytes of the operation.

  "
  (let* ((opcode (mref nes address))
         (op (aref *opcode-data* opcode)))
    (with-op (op)
      (cond
        ((not legal) (values '(ILLEGAL) "???" (list opcode)))
        ((not (has-operand-p op)) (values `(,name) (string name) (list opcode)))
        (t (let* ((operand-width (get addressing-mode 'operand-width))
                  (operand (operand-at nes (1+/16 address) operand-width)))
             (values `(,name (,addressing-mode ,operand))
                     (format nil "~A ~A"
                             name
                             (render-addressing-mode-operand addressing-mode
                                                             operand))
                     (iterate (repeat width)
                              (for i :from address) ; todo should be 1+/16...
                              (collect (mref nes i))))))))))

(defun pretty-print-instruction-at
    (nes address &optional (stream *standard-output*))
  (multiple-value-bind (op-list op-string bytes)
      (disassemble-op nes address)
    (declare (ignore op-list))
    (values (format stream "~4,'0X  ~9A ~A"
                    address
                    (format nil "~{~2,'0X~^ ~}" bytes)
                    op-string)
            (length bytes))))

(defun disassemble-chunk (nes address)
  (iterate (repeat 24)
           (incf address (nth-value 1 (pretty-print-instruction-at nes address))))
  address)

(defun disassemble-interactively (nes starting-address)
  (iterate
    (for address :first starting-address :then next-address)
    (for next-address = (disassemble-chunk nes address))
    (terpri)
    (for line = (read-line))
    (cond
      ((string= line "") nil)
      ((string= line "q") (return))
      (t (setf next-address (parse-integer line :radix 16))))))


(defun log-state (nes)
  (with-nes (nes)
    (format t "~47A A:~2,'0X X:~2,'0X Y:~2,'0X P:~2,'0X SP:~2,'0X "
            (pretty-print-instruction-at nes pc nil)
            a x y status sp)
    (force-output)))


;; C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FD PPU:  0,  0 CYC:7
;; C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FDq
;; C000  4C F5 C5   JMP $C5F5                      A:00 X:00 Y:00 P:24 SP:FD
