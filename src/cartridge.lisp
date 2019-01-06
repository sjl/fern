(in-package :fern)

(defgeneric make-cartridge (id prg chr ram))

(defstruct (cartridge (:constructor nil))
  (id (required 'id) :type (or null (integer 0 256)))
  (reader #'unimplemented :type function-designator)
  (writer #'unimplemented :type function-designator)
  (prg)
  (chr)
  (wram))
