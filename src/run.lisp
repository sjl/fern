(in-package :fern)

(defun load-cartridge-into-nes (nes path)
  (with-open-file (rom path :direction :input :element-type 'u8)
    (multiple-value-bind (id mirroring-type battery-backed? trainer prg chr ram)
        (read-ines rom)
      (declare (ignore mirroring-type trainer battery-backed?))
      (setf (cartridge nes)
            (make-cartridge id prg chr ram)))))

(defun run-one (nes)
  (let ((opcode-function (-<> (pc nes)
                           (mref nes <>)
                           (aref *opcode-functions* <>))))
    (incf/16 (pc nes))
    (funcall opcode-function nes)))

(defun run ()
  (let ((nes (make-nes)))
    (load-cartridge-into-nes nes "/home/sjl/src/nes-test-roms/nestest.nes")
    (reset nes)
    (setf (pc nes) #xC000) ; fuckin nestest
    (iterate
      (repeat 1000)
      (log-state nes)
      (terpri)
      (run-one nes))))

;; (with-open-file (*standard-output* "test/output.log" :direction :output :if-exists :supersede)
;;   (run))
