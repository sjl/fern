(in-package :fern)

(defstruct (cartridge-0 (:include cartridge))
  (program nil :type memory)
  (character nil :type (memory #x2000))
  (save-ram nil :type (memory #x2000)))

(defmethod make-cartridge ((id (eql 0)) prg chr ram)
  (check-bank-length 0 "PRG ROM" prg 1 2)
  (check-bank-length 0 "CHR ROM" chr 1)
  (check-bank-length 0 "Save RAM" ram 1)
  (make-cartridge-0
    :id 0
    :program (apply #'concatenate 'memory prg)
    :character (first chr)
    :save-ram (first ram)))


(defmethod cartridge-read-program ((cartridge cartridge-0) address)
  (address-case (index address)
    (#x6000 #x00) ;; what should this actually do?
    (#x8000 (aref (cartridge-0-save-ram cartridge) index))
    (#x10000 (aref% (cartridge-0-program cartridge) index))
    (t (error "Address ~2,'0X out of range for reading from cartridge type 0." address))))

(defmethod cartridge-write-program ((cartridge cartridge-0) address value)
  (address-case (index address)
    (#x6000 nil) ;; what should this actually do?
    (#x8000 (setf (aref (cartridge-0-save-ram cartridge) index) value))
    (#x10000 (setf (aref% (cartridge-0-program cartridge) index) value))
    (t (error "Address ~2,'0X out of range for writing to cartridge type 0." address))))


(defmethod cartridge-read-character ((cartridge cartridge-0) address)
  (check-type address (integer #x0000 (#x2000)))
  (aref (cartridge-0-character cartridge) address))

(defmethod cartridge-write-character ((cartridge cartridge-0) address value)
  (check-type address (integer #x0000 (#x2000)))
  (setf (aref (cartridge-0-character cartridge) address) value))
