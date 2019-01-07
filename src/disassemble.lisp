(in-package :fern)

(defun retrieve-instruction (nes address)
  (let* ((opcode (mref nes (wrap address 16)))
         (op (aref *opcode-data* opcode)))
    (iterate (repeat (op-width op))
             (for a :from address)
             (collect (mref nes (wrap a 16))))))

(defun operand-bytes-to-operand (bytes)
  (ecase (length bytes)
    (0 nil)
    (1 (first bytes))
    (2 (cat (first bytes) (second bytes)))))

(defun instruction-sexp (instruction)
  (destructuring-bind (opcode &rest operand-bytes) instruction
    (let ((op (aref *opcode-data* opcode)))
      (cons (op-name op)
            (render-operand-to-list
              (op-addressing-mode op)
              (operand-bytes-to-operand operand-bytes))))))

(defun instruction-string (instruction)
  (destructuring-bind (opcode &rest operand-bytes) instruction
    (let ((op (aref *opcode-data* opcode)))
      (format nil "~C~A ~A"
              (if (op-legal op) #\Space #\*)
              (op-name op)
              (render-operand-to-string
                (op-addressing-mode op)
                (operand-bytes-to-operand operand-bytes))))))

(defun instruction-nestest (instruction)
  (destructuring-bind (opcode &rest operand-bytes) instruction
    (declare (ignore operand-bytes))
    (let ((op (aref *opcode-data* opcode)))
      (format nil "~C~A ~A"
              (if (op-legal op) #\Space #\*)
              (op-name op)
              (render-operand-to-nestest-string
                (op-addressing-mode op)
                *nes*))))) ; fuck this

(defun pretty-print-instruction-at (nes address)
  (let ((instruction (retrieve-instruction nes address)))
    (format t "~4,'0X  ~9A~A"
            address
            (format nil "~{~2,'0X~^ ~}" instruction)
            (instruction-nestest instruction))
    (length instruction)))


(defun disassemble-chunk (nes address)
  (iterate (repeat 24)
           (incf address (pretty-print-instruction-at nes address))
           (terpri))
  address)

(defun disassemble-interactively (nes starting-address)
  (iterate
    (for address :first starting-address :then next-address)
    (for next-address = (disassemble-chunk nes address))
    (for line = (read-line))
    (cond
      ((string= line "") nil)
      ((string= line "q") (return))
      (t (setf next-address (parse-integer line :radix 16))))))


(defun log-state (nes)
  (with-nes (nes)
    (format t "~47A A:~2,'0X X:~2,'0X Y:~2,'0X P:~2,'0X SP:~2,'0X "
            (with-output-to-string (*standard-output*)
              (pretty-print-instruction-at nes pc))
            a x y status sp)
    (force-output)))


