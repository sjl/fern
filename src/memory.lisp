(in-package :fern)

(defun mirrored-address (start end address)
  "Return the base address in `start` to `end` after mirroring.

  The NES has several regions of memory that are mirrored, e.g.:

    total  2000 +--------------+        4000 +---------------+
                | Mirror of    |             | Mirror of     |
                | 0000 - 0800  |             | 2000 - 2008   |
    end    0800 +--------------+        2008 +---------------+
                | Internal RAM |             | I/O Registers |
    start  0000 +--------------+        2000 +---------------+

  This function will take an address between `start` and `total` (not checked)
  and return the appropriate address between `start` and `end` to use.

  "
  (+ start (mod address (- end start))))


;;;; Read ---------------------------------------------------------------------
(defun read-internal-memory (nes address)
  (aref (ram nes) (mirrored-address #x0000 #x800 address)))

(defun read-low-i/o-register (nes address)
  (return-from read-low-i/o-register #x00) ;; TODO
  (ecase (mirrored-address #x2000 #x2008 address)
    (#x2000 (read-ppu/control-0 nes))
    (#x2001 (read-ppu/control-1 nes))
    (#x2002 (read-ppu/status nes))
    (#x2003 (read-ppu/sprite-ram-address nes))
    (#x2004 (read-ppu/sprite-ram-i/o nes))
    (#x2005 (read-ppu/vram-address-1 nes))
    (#x2006 (read-ppu/vram-address-2 nes))
    (#x2007 (read-ppu/vram-i/o-2 nes))))

(defun read-apu (nes address)
  (return-from read-apu #x00) ;; TODO
  (ecase address
    (#x4000 (read-apu/pulse-1-control nes))
    (#x4001 (read-apu/pulse-1-ramp-control nes))
    (#x4002 (read-apu/pulse-1-fine-tune nes))
    (#x4003 (read-apu/pulse-1-coarse-tune nes))
    (#x4004 (read-apu/pulse-2-control nes))
    (#x4005 (read-apu/pulse-2-ramp-control nes))
    (#x4006 (read-apu/pulse-2-fine-tune nes))
    (#x4007 (read-apu/pulse-2-coarse-tune nes))
    (#x4008 (read-apu/triangle-control-1 nes))
    (#x4009 (read-apu/triangle-control-2 nes))
    (#x400A (read-apu/triangle-frequency-1 nes))
    (#x400B (read-apu/triangle-frequency-2 nes))
    (#x400C (read-apu/noise-control nes))
    (#x400D #x00) ;; unused
    (#x400F (read-apu/noise-frequency-2 nes))
    (#x4010 (read-apu/delta-modulation-control nes))
    (#x4011 (read-apu/delta-modulation-d/a nes))
    (#x4012 (read-apu/delta-modulation-address nes))
    (#x4013 (read-apu/delta-modulation-data-length nes))
    (#x4015 (read-apu/sound-channels nes))))

(defun read-high-i/o-register (nes address)
  (return-from read-high-i/o-register #x00) ;; TODO
  (cond
    ((<= address #x4013) (read-apu nes address))
    ((= address #x4014) (read-ppu/sprite-dma nes))
    ((= address #x4015) (read-apu nes address))
    ((= address #x4016) (read-joypad-1 nes))
    ((= address #x4017) (read-joypad-2 nes)))
  )

(defun read-test-mode-register (nes address)
  (declare (ignore nes))
  (error "Test mode address ~4,'0X read." address))


(defun internal-read (nes address)
  (cond
    ((< address #x2000) (read-internal-memory nes address))
    ((< address #x4000) (read-low-i/o-register nes address))
    ((< address #x4018) (read-high-i/o-register nes address))
    ((< address #x4020) (read-test-mode-register nes address))
    (t (error "Address ~4,'0X out of range for internal memory read." address))))

(defun cartridge-read (nes address)
  (let ((cartridge (cartridge nes)))
    (funcall (cartridge-reader cartridge) cartridge address)))


;;;; Write --------------------------------------------------------------------
(defun write-internal-memory (nes address value)
  (setf (aref (ram nes) (mirrored-address #x0000 #x800 address)) value))

(defun write-low-i/o-register (nes address value)
  (return-from write-low-i/o-register) ;; TODO
  (ecase (mirrored-address #x2000 #x2008 address)
    (#x2000 (write-ppu/control-0 nes value))
    (#x2001 (write-ppu/control-1 nes value))
    (#x2002 (write-ppu/status nes value))
    (#x2003 (write-ppu/sprite-ram-address nes value))
    (#x2004 (write-ppu/sprite-ram-i/o nes value))
    (#x2005 (write-ppu/vram-address-1 nes value))
    (#x2006 (write-ppu/vram-address-2 nes value))
    (#x2007 (write-ppu/vram-i/o-2 nes value))))

(defun write-apu (nes address value)
  (return-from write-apu #x00) ;; TODO
  (ecase address
    (#x4000 (write-apu/pulse-1-control nes value))
    (#x4001 (write-apu/pulse-1-ramp-control nes value))
    (#x4002 (write-apu/pulse-1-fine-tune nes value))
    (#x4003 (write-apu/pulse-1-coarse-tune nes value))
    (#x4004 (write-apu/pulse-2-control nes value))
    (#x4005 (write-apu/pulse-2-ramp-control nes value))
    (#x4006 (write-apu/pulse-2-fine-tune nes value))
    (#x4007 (write-apu/pulse-2-coarse-tune nes value))
    (#x4008 (write-apu/triangle-control-1 nes value))
    (#x4009 (write-apu/triangle-control-2 nes value))
    (#x400A (write-apu/triangle-frequency-1 nes value))
    (#x400B (write-apu/triangle-frequency-2 nes value))
    (#x400C (write-apu/noise-control nes value))
    (#x400D #x00) ;; unused
    (#x400F (write-apu/noise-frequency-2 nes value))
    (#x4010 (write-apu/delta-modulation-control nes value))
    (#x4011 (write-apu/delta-modulation-d/a nes value))
    (#x4012 (write-apu/delta-modulation-address nes value))
    (#x4013 (write-apu/delta-modulation-data-length nes value))
    (#x4015 (write-apu/sound-channels nes value))))

(defun write-high-i/o-register (nes address value)
  (return-from write-high-i/o-register #x00) ;; TODO
  (cond
    ((<= address #x4013) (write-apu nes address value))
    ((= address #x4014) (write-ppu/sprite-dma nes value))
    ((= address #x4015) (write-apu nes address value))
    ((= address #x4016) (write-joypad-1 nes value))
    ((= address #x4017) (write-joypad-2 nes value))))

(defun write-test-mode-register (nes address value)
  (declare (ignore nes))
  (error "Test mode address ~4,'0X written with value ~2,'0X." address value))


(defun internal-write (nes address value)
  (cond
    ((< address #x2000) (write-internal-memory nes address value))
    ((< address #x4000) (write-low-i/o-register nes address value))
    ((< address #x4018) (write-high-i/o-register nes address value))
    ((< address #x4020) (write-test-mode-register nes address value))
    (t (error "Address ~4,'0X out of range for internal memory write." address)))
  nil)

(defun cartridge-write (nes address value)
  (let ((cartridge (cartridge nes)))
    (funcall (cartridge-writer cartridge) cartridge address value))
  nil)


;;;; API ----------------------------------------------------------------------
(defun mref (nes address)
  (if (< address #x4020)
    (internal-read nes address)
    (cartridge-read nes address)))

(defun (setf mref) (new-value nes address)
  (if (< address #x4020)
    (internal-write nes address new-value)
    (cartridge-write nes address new-value))
  new-value)


(defun mref/16 (nes address)
  (cat (mref nes address)
       (mref nes (1+/16 address))))


;;;; Stack --------------------------------------------------------------------
(defun-inline stack-address (nes)
  (logior #x0100 (sp nes)))


(defun stack-push (nes value)
  (setf (mref nes (stack-address nes)) value)
  (decf/8 (sp nes)))

(defun stack-push/16 (nes value)
  (stack-push nes (msb value))
  (stack-push nes (lsb value)))


(defun stack-pop (nes)
  (incf/8 (sp nes))
  (mref nes (stack-address nes)))

(defun stack-pop/16 (nes)
  (cat (stack-pop nes)
       (stack-pop nes)))

