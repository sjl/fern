(in-package :fern)

(defstruct (cartridge-empty (:include cartridge)))

(defun cartridge-empty/read (cartridge address)
  (declare (ignore cartridge address))
  (error "Cannot read from empty cartridge."))

(defun cartridge-empty/write (cartridge address value)
  (declare (ignore cartridge address value))
  (error "Cannot read from empty cartridge."))

(defmethod make-cartridge ((id null) prg chr ram)
  (make-cartridge-empty :id nil
                        :reader 'cartridge-empty/read
                        :writer 'cartridge-empty/write))
