(in-package :fern)

(defstruct (cartridge-0 (:include cartridge)))

(defun cartridge-0/read (cartridge address)
  (if (< address #x6000) ;; todo fix this shit
    0
    (progn
      (assert (in-range-p #x6000 address #x10000) ()
        "Address ~2,'0X out of range for reading from cartridge." address)
      (decf address #x6000)
      (if (< address #x2000)
        (aref (cartridge-wram cartridge) address)
        (progn
          (decf address #x2000)
          (aref (cartridge-prg cartridge)
                (mod address (array-total-size (cartridge-prg cartridge)))))))))

(defun cartridge-0/write (cartridge address value)
  (declare (ignore cartridge address value))
  (TODO))

(defmethod make-cartridge ((id (eql 0)) prg chr ram)
  (make-cartridge-0 :id 0
                    :prg (apply #'concatenate 'memory prg)
                    :chr (apply #'concatenate 'memory chr)
                    :wram (apply #'concatenate 'memory ram)
                    :reader 'cartridge-0/read
                    :writer 'cartridge-0/write))
