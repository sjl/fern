(in-package :fern)

;;;; MMUs and Mappers ---------------------------------------------------------
(deftype memory (size)
  `(simple-array u8 (,size)))

(defun make-memory (size)
  (make-array size :initial-element 0 :element-type 'u8))

(defun unimplemented (&rest args)
  (declare (ignore args))
  (error "Unimplemented reader/writer MMU."))


(defstruct mmu
  (ram (make-memory #x0800) :type (memory #x0800))
  (wram nil :type (memory #x2000))
  (prg-rom nil :type (memory *))
  (chr-rom nil :type (memory *))
  (reader #'unimplemented :type function-designator)
  (writer #'unimplemented :type function-designator))


(defun basic-read (mmu address)
  (if (< address #x2000)
    (aref (mmu-ram mmu) (mod address #x800))
    (TODO)))

(defun basic-write (mmu address value)
  (if (< address #x2000)
    (setf (aref (mmu-ram mmu) (mod address #x800)) value)
    (TODO)))


(defun mmu-0-read (mmu address)
  (if (< address #x4020)
    (basic-read mmu address)
    (TODO)))

(defun mmu-0-write (mmu address value)
  (if (< address #x4020)
    (basic-write mmu address value)
    (TODO)))


(defun mref (mmu address)
  (funcall (mmu-reader mmu) address))

(defun (setf mref) (new-value mmu address)
  (funcall (mmu-writer mmu) address new-value)
  new-value)


(defun make-mmu-0 (prg-banks chr-banks ram-banks)
  (make-mmu
    :prg-rom (make-memory (kb (* 16 prg-banks)))
    :chr-rom (make-memory (kb (* 8 chr-banks)))
    :wram (if ram-banks
            (make-memory (kb 8))
            (make-memory 0))
    :reader 'mmu-0-read
    :writer 'mmu-0-write))


;;;; NES ----------------------------------------------------------------------
(defstruct nes
  (running t :type boolean)
  (a 0 :type u8)
  (x 0 :type u8)
  (y 0 :type u8)
  (pc 0 :type u16)
  (sp #x01FF :type u16)
  (mmu nil :type mmu))


;;;; iNES File Format ---------------------------------------------------------
(defun parse-ines (vector)
  "Take a raw iNES byte vector and parse its contents.

  Returns seven values:

  * Mapper number
  * Mirroring type
  * Battery-backed?
  * Trainer
  * PRG ROM banks
  * CHR ROM banks
  * RAM banks

  "
  (let ((chunk-start 0))
    (flet ((read-chunk (length)
             (let ((result (make-memory length)))
               (replace result vector :start2 chunk-start)
               (incf chunk-start length)
               result)))
      (let ((header (read-chunk 16)))
        (assert (string= "NES" (map 'string #'code-char (subseq header 0 3))) ()
          "Cannot parse file as iNES (missing NES in header ~S)." header)
        (assert (= #x1A (aref header 3)) ()
          "Cannot parse file as iNES (missing magic byte in header ~S)." header)
        (let* ((prg-rom-banks (aref header 4))
               (chr-rom-banks (aref header 5))
               (rom-control-1 (aref header 6))
               (rom-control-2 (aref header 7))
               (ram-banks (aref header 8))
               (reserved (subseq header 9))
               (mirroring (if (logbitp 3 rom-control-1)
                            :four-screen
                            (if (logbitp 0 rom-control-1)
                              :vertical
                              :horizontal)))
               (battery-backed (logbitp 1 rom-control-1))
               (has-trainer (logbitp 2 rom-control-1))
               (mapper (cat-bytes 4
                                  (bits rom-control-1 4 7)
                                  (bits rom-control-2 4 7)))
               (reserved-bits (bits rom-control-2 0 3))
               (trainer nil))
          (assert (every #'zerop reserved) ()
            "Cannot parse iNES file (unknown data in reserved area: ~S)." reserved)
          (assert (zerop reserved-bits) ()
            "Cannot parse iNES file (unknown data in reserved bits: ~S)." reserved-bits)
          (when has-trainer
            (setf trainer (read-chunk 512)))
          (values mapper mirroring battery-backed trainer
                  (iterate (repeat prg-rom-banks)
                           (collect (read-chunk (kb 16))))
                  (iterate (repeat chr-rom-banks)
                           (collect (read-chunk (kb 16))))
                  (iterate (repeat (if (zerop ram-banks) 1 ram-banks)) ; bc
                           (collect (make-memory (kb 8))))))))))


;;;; Opcodes ------------------------------------------------------------------
(defun opcode-illegal (opcode)
  (error "Illegal opcode: ~2,'0X" opcode))

(defparameter *debug-opcodes* t)

(defparameter *opcode-functions*
  (iterate (for opcode :from 0 :below 256)
           (collect (curry #'opcode-illegal opcode) :result-type vector)))

(defparameter *opcode-data*
  (iterate (for opcode :from 0 :below 256)
           (collect `(:opcode ,opcode :doc "Illegal opcode")
                    :result-type vector)))


(defmacro define-opcode (opcode name arglist documentation &body body)
  (let ((function-name (symb 'opcode- name)))
    `(progn
       (defun ,function-name ,arglist ,documentation ,@body)
       (setf (aref *opcode-functions* ,opcode)
             ,(if *debug-opcodes* `',function-name `#',function-name)
             (aref *opcode-data* ,opcode)
             `(:opcode ,,opcode :doc ,,documentation))
       ',name)))


(define-opcode #x42 foo (x y)
  "this is a test"
  (prl x y))

