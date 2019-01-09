(in-package :fern)


;;;; Read ---------------------------------------------------------------------
(defun read-internal-memory (nes address)
  (aref (ram nes) (mirrored-address #x0000 #x800 address)))

(defun read-joypad-1 (nes)
  (TODO))

(defun read-joypad-2 (nes)
  (TODO))

(defun read-apu (nes address)
  (let ((apu (apu nes)))
    (ecase address
      (#x4000 (read-apu/pulse-1-control apu))
      (#x4001 (read-apu/pulse-1-ramp-control apu))
      (#x4002 (read-apu/pulse-1-fine-tune apu))
      (#x4003 (read-apu/pulse-1-coarse-tune apu))
      (#x4004 (read-apu/pulse-2-control apu))
      (#x4005 (read-apu/pulse-2-ramp-control apu))
      (#x4006 (read-apu/pulse-2-fine-tune apu))
      (#x4007 (read-apu/pulse-2-coarse-tune apu))
      (#x4008 (read-apu/triangle-control-1 apu))
      (#x4009 (read-apu/triangle-control-2 apu))
      (#x400A (read-apu/triangle-frequency-1 apu))
      (#x400B (read-apu/triangle-frequency-2 apu))
      (#x400C (read-apu/noise-control apu))
      (#x400D #x00) ;; unused
      (#x400F (read-apu/noise-frequency-2 apu))
      (#x4010 (read-apu/delta-modulation-control apu))
      (#x4011 (read-apu/delta-modulation-d/a apu))
      (#x4012 (read-apu/delta-modulation-address apu))
      (#x4013 (read-apu/delta-modulation-data-length apu))
      (#x4015 (read-apu/sound-channels apu)))))

(defun read-sprite-dma (nes)
  (read-ppu/undefined (ppu nes)))

(defun read-i/o-register (nes address)
  (cond
    ((<= address #x4013) (read-apu nes address))
    ((= address #x4014) (read-sprite-dma nes))
    ((= address #x4015) (read-apu nes address))
    ((= address #x4016) (read-joypad-1 nes))
    ((= address #x4017) (read-joypad-2 nes))))

(defun read-test-mode-register (nes address)
  (declare (ignore nes))
  (error "Test mode address ~4,'0X read." address))


(defun internal-read (nes address)
  (cond
    ((< address #x2000) (read-internal-memory nes address))
    ((< address #x4000) (read-ppu (ppu nes) address))
    ((< address #x4018) (read-i/o-register nes address))
    ((< address #x4020) (read-test-mode-register nes address))
    (t (error "Address ~4,'0X out of range for internal memory read." address))))


;;;; Write --------------------------------------------------------------------
(defun write-internal-memory (nes address value)
  (setf (aref (ram nes) (mirrored-address #x0000 #x800 address)) value))

(defun write-joypad-1 (nes value)
  (TODO))

(defun write-joypad-2 (nes value)
  (TODO))

(defun write-apu (nes address value)
  (let ((apu (apu nes)))
    (ecase address
      (#x4000 (write-apu/pulse-1-control apu value))
      (#x4001 (write-apu/pulse-1-ramp-control apu value))
      (#x4002 (write-apu/pulse-1-fine-tune apu value))
      (#x4003 (write-apu/pulse-1-coarse-tune apu value))
      (#x4004 (write-apu/pulse-2-control apu value))
      (#x4005 (write-apu/pulse-2-ramp-control apu value))
      (#x4006 (write-apu/pulse-2-fine-tune apu value))
      (#x4007 (write-apu/pulse-2-coarse-tune apu value))
      (#x4008 (write-apu/triangle-control-1 apu value))
      (#x4009 (write-apu/triangle-control-2 apu value))
      (#x400A (write-apu/triangle-frequency-1 apu value))
      (#x400B (write-apu/triangle-frequency-2 apu value))
      (#x400C (write-apu/noise-control apu value))
      (#x400D #x00) ;; unused
      (#x400F (write-apu/noise-frequency-2 apu value))
      (#x4010 (write-apu/delta-modulation-control apu value))
      (#x4011 (write-apu/delta-modulation-d/a apu value))
      (#x4012 (write-apu/delta-modulation-address apu value))
      (#x4013 (write-apu/delta-modulation-data-length apu value))
      (#x4015 (write-apu/sound-channels apu value)))))

(defun write-sprite-dma (nes value)
  ;; http://wiki.nesdev.com/w/index.php/PPU_registers#OAMDMA
  (iterate
    (with sprite-ram = (sprite-ram (ppu nes)))
    (for source :from (cat #x00 value) :to (cat #xFF value))
    (for dest :from #x00) ; technically we should start from sprite-ram-address…
    (setf (aref sprite-ram dest) (mref nes source)))
  ;; > The CPU is suspended during the transfer, which will take 513 or 514
  ;; > cycles after the $4014 write tick. (1 dummy read cycle while waiting for
  ;; > writes to complete, +1 if on an odd CPU cycle, then 256 alternating
  ;; > read/write cycles.)
  (incf (cycles nes)
        (if (oddp (cycles nes))
          514
          513)))

(defun write-i/o-register (nes address value)
  (cond
    ((<= address #x4013) (write-apu nes address value))
    ((= address #x4014) (write-sprite-dma nes value))
    ((= address #x4015) (write-apu nes address value))
    ((= address #x4016) (write-joypad-1 nes value))
    ((= address #x4017) (write-joypad-2 nes value))))

(defun write-test-mode-register (nes address value)
  (declare (ignore nes))
  (error "Test mode address ~4,'0X written with value ~2,'0X." address value))


(defun internal-write (nes address value)
  (cond
    ((< address #x2000) (write-internal-memory nes address value))
    ((< address #x4000) (write-ppu (ppu nes) address value))
    ((< address #x4018) (write-i/o-register nes address value))
    ((< address #x4020) (write-test-mode-register nes address value))
    (t (error "Address ~4,'0X out of range for internal memory write." address))))


;;;; API ----------------------------------------------------------------------
(defun mref (nes address)
  (if (< address #x4020)
    (internal-read nes address)
    (cartridge-read-program (cartridge nes) address)))

(defun (setf mref) (new-value nes address)
  (if (< address #x4020)
    (internal-write nes address new-value)
    (cartridge-write-program nes address new-value))
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

