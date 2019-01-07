(in-package :fern)

(defvar *nes* nil)


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

(defun step/log (nes step log)
  (when (or log step)
    (log-state nes)
    (if step
      (progn (force-output) (read-line))
      (terpri))))


(defun run (path &key limit initial-pc step log)
  (let ((nes (make-nes)))
    (setf *nes* nes)
    (load-cartridge-into-nes nes path)
    (reset nes)
    (when initial-pc
      (setf (pc nes) initial-pc))
    (iterate
      (with remaining = limit)
      (when remaining
        (if (zerop remaining)
          (return)
          (decf remaining)))
      (step/log nes step log)
      (execute-instruction nes))))

(defun run-nestest ()
  (with-open-file (*standard-output* "test/output.log"
                                     :direction :output
                                     :if-exists :supersede)
    (run "test/nestest.nes" :limit 8991 :initial-pc #xC000 :step nil :log t)))
