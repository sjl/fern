(in-package :fern)

(defgeneric make-cartridge (id prg chr ram))

(defstruct (cartridge (:constructor nil))
  (id (required 'id) :type (or null (integer 0 256))))

;; TODO: think about making these non-generic and embedding them in the struct,
;; if this is too slow...

(defgeneric cartridge-read-program (cartridge address))
(defgeneric cartridge-write-program (cartridge address value))

(defgeneric cartridge-read-character (cartridge address))
(defgeneric cartridge-write-character (cartridge address value))



(defmacro check-bank-length (cartridge-id name banks &rest acceptable-lengths)
  "Check that `banks` contains an acceptable number of memory banks.

  Example:

    (check-bank-length 0 \"PRG ROM\" prg 1 2)
    (check-bank-length 0 \"CHR ROM\" chr 1)
    (check-bank-length 0 \"Save RAM\" ram 1)

  "
  (once-only (banks)
    (with-gensyms (acceptable-lengths%)
      `(let ((,acceptable-lengths% ',acceptable-lengths))
         (assert (member (length ,banks) ,acceptable-lengths%) ()
           "Invalid number of ~A banks for cartridge type ~A (expected one of ~A, got ~D)."
           ,name
           ,cartridge-id
           ,acceptable-lengths%
           (length ,banks))))))
