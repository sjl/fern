(in-package :fern)

(defstruct (cartridge-0 (:include cartridge)))

(defun cartridge-0/read (cartridge address)
  (declare (ignore cartridge address))
  (TODO))

(defun cartridge-0/write (cartridge address value)
  (declare (ignore cartridge address value))
  (TODO))

(defmethod make-cartridge ((id (eql 0)) prg chr ram)
  (make-cartridge-0 :id 0
                    :prg prg
                    :chr chr
                    :wram ram
                    :reader 'cartridge-0/read
                    :writer 'cartridge-0/write))
