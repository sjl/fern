(in-package :fern)

(defvar *nes* nil)
(defvar *log* nil)
(defvar *step* nil)


(defun load-cartridge-into-nes (nes path)
  (with-open-file (rom path :direction :input :element-type 'u8)
    (multiple-value-bind (id mirroring-type battery-backed? trainer prg chr ram)
        (read-ines rom)
      (declare (ignore mirroring-type trainer battery-backed?))
      ;; todo trainer and all the other shit here
      (setf (cartridge nes)
            (make-cartridge id prg chr ram)))))

(defun execute-instruction (nes)
  (let ((opcode-function (-<> (pc nes)
                           (mref nes <>)
                           (aref *opcode-functions* <>))))
    (incf/16 (pc nes))
    (funcall opcode-function nes)))

(defun step/log (nes)
  (when (or *log* *step*)
    (log-state nes)
    (if *step*
      (progn (force-output) (read-line))
      (terpri))))


(defun run (path)
  (let ((nes (make-nes)))
    (setf *nes* nes)
    (load-cartridge-into-nes nes path)
    (reset nes)
    (iterate (step/log nes)
             (execute-instruction nes))))
