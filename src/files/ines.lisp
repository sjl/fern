(in-package :fern)

(defun read-ines (stream)
  "Read an iNES ROM from `stream`, which must be a binary input stream.

  Returns seven values:

  * Mapper number
  * Mirroring type
  * Battery-backed?
  * Trainer
  * PRG ROM banks
  * CHR ROM banks
  * RAM banks

  "
  (assert (string= "NES" (read-string-from-stream stream 3)) ()
    "Cannot parse file as iNES (missing NES in header).")
  (assert (= #x1A (read-byte stream)) ()
    "Cannot parse file as iNES (missing magic byte in header).")
  (let* ((prg-rom-banks (read-byte stream))
         (chr-rom-banks (read-byte stream))
         (rom-control-1 (read-byte stream))
         (rom-control-2 (read-byte stream))
         (ram-banks (read-byte stream))
         (reserved (read-memory-from-stream stream 7))
         (mirroring (if (logbitp 3 rom-control-1)
                      :four-screen
                      (if (logbitp 0 rom-control-1)
                        :vertical
                        :horizontal)))
         (battery-backed (logbitp 1 rom-control-1))
         (has-trainer (logbitp 2 rom-control-1))
         (mapper (cat (bits rom-control-1 4 7)
                      (bits rom-control-2 4 7)
                      4))
         (reserved-bits (bits rom-control-2 0 3))
         (trainer nil))
    (assert (every #'zerop reserved) ()
      "Cannot parse iNES file (unknown data in reserved area: ~S)." reserved)
    (assert (zerop reserved-bits) ()
      "Cannot parse iNES file (unknown data in reserved bits: ~S)." reserved-bits)
    (when has-trainer
      (setf trainer (read-memory-from-stream stream 512)))
    (multiple-value-prog1
      (values mapper mirroring battery-backed trainer
              (iterate (repeat prg-rom-banks)
                       (collect (read-memory-from-stream stream (kb 16))))
              (iterate (repeat chr-rom-banks)
                       (collect (read-memory-from-stream stream (kb 8))))
              (iterate (repeat (if (zerop ram-banks) 1 ram-banks)) ; bc
                       (collect (make-memory (kb 8)))))
      (assert-eof stream "Garbage data found in INES ROM file."))))
