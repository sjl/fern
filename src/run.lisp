(in-package :fern)

(defvar *nes* nil)
(defparameter *breakpoints* (make-array #xFFFF :initial-element nil))
(defparameter *step* t)
(defparameter *log* nil)


(defun load-cartridge-into-nes (nes path)
  (with-open-file (rom path :direction :input :element-type 'u8)
    (multiple-value-bind (id mirroring-type battery-backed? trainer prg chr ram)
        (read-ines rom)
      (declare (ignore mirroring-type trainer battery-backed?))
      ;; todo trainer and all the other shit here
      (let ((cartridge (make-cartridge id prg chr ram)))
        (setf (cartridge nes) cartridge
              (ppu-cartridge (ppu nes)) cartridge)))))

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

(defun add-breakpoint (address)
  (setf (aref *breakpoints* address) t))

(defun remove-breakpoint (address)
  (setf (aref *breakpoints* address) nil))

(defun check-breakpoint (nes)
  (when (aref *breakpoints* (pc nes))
    (pr 'break)
    (setf *step* t)))


(defun run (path &key limit initial-pc)
  (let ((nes (make-nes))
        (*step* *step*)
        (*log* *log*))
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
      (check-breakpoint nes)
      (step/log nes)
      (execute-instruction nes))))

(defun run-nestest ()
  (with-open-file (*standard-output* "test/output.log"
                                     :direction :output
                                     :if-exists :supersede)
    (let ((*step* nil)
          (*log* t))
      (run "test/nestest.nes" :limit 8991 :initial-pc #xC000))))


;; (setf *print-circle* t
;;       *print-length* 25)

;; (setf
;;   (pref (ppu *nes*) #x0000)  #b01000001
;;   (pref (ppu *nes*) #x0001)  #b11000010
;;   (pref (ppu *nes*) #x0002)  #b01000100
;;   (pref (ppu *nes*) #x0003)  #b01001000
;;   (pref (ppu *nes*) #x0004)  #b00010000
;;   (pref (ppu *nes*) #x0005)  #b00100000
;;   (pref (ppu *nes*) #x0006)  #b01000000
;;   (pref (ppu *nes*) #x0007)  #b10000000
;;   (pref (ppu *nes*) #x0008)  #b00000001
;;   (pref (ppu *nes*) #x0009)  #b00000010
;;   (pref (ppu *nes*) #x000A)  #b00000100
;;   (pref (ppu *nes*) #x000B)  #b00001000
;;   (pref (ppu *nes*) #x000C)  #b00010110
;;   (pref (ppu *nes*) #x000D)  #b00100001
;;   (pref (ppu *nes*) #x000E)  #b01000010
;;   (pref (ppu *nes*) #x000F)  #b10000111)

;; (setf
;;   (pref (ppu *nes*) #x1000)  #b01110000
;;   (pref (ppu *nes*) #x1001)  #b00000010
;;   (pref (ppu *nes*) #x1002)  #b01000100
;;   (pref (ppu *nes*) #x1003)  #b01001000
;;   (pref (ppu *nes*) #x1004)  #b00010000
;;   (pref (ppu *nes*) #x1005)  #b00100000
;;   (pref (ppu *nes*) #x1006)  #b01000000
;;   (pref (ppu *nes*) #x1007)  #b10000000
;;   (pref (ppu *nes*) #x1008)  #b00000000
;;   (pref (ppu *nes*) #x1009)  #b00000010
;;   (pref (ppu *nes*) #x100A)  #b00000100
;;   (pref (ppu *nes*) #x100B)  #b00001000
;;   (pref (ppu *nes*) #x100C)  #b00010110
;;   (pref (ppu *nes*) #x100D)  #b00100001
;;   (pref (ppu *nes*) #x100E)  #b01000010
;;   (pref (ppu *nes*) #x100F)  #b10000111)
