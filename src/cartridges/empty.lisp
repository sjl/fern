(in-package :fern)

(defstruct (cartridge-empty (:include cartridge)))

(defmethod make-cartridge ((id null) prg chr ram)
  (make-cartridge-empty :id nil))

(defmethod cartridge-read-program ((cartridge cartridge-empty) address)
  (error "Cannot read PRG from empty cartridge."))

(defmethod cartridge-write-program ((cartridge cartridge-empty) address value)
  (error "Cannot write PRG to empty cartridge."))

(defmethod cartridge-read-character ((cartridge cartridge-empty) address)
  (error "Cannot read CHR from empty cartridge."))

(defmethod cartridge-write-character ((cartridge cartridge-empty) address value)
  (error "Cannot write CHR to empty cartridge."))
