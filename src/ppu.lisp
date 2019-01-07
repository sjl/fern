(in-package :fern)

;;;; PPU ----------------------------------------------------------------------
(defstruct (ppu (:conc-name nil))
  (palette-ram (make-memory #x20) :type (memory #x20))
  (sprite-ram (make-memory #x100) :type (memory #x100))
  (video-ram (make-memory #x1000) :type (memory #x1000))
  (sprite-address #x00 :type u8)
  (scroll-x 0 :type u8)
  (scroll-y 0 :type u8)
  (video-ram-address #x00 :type u16)
  (temporary-address #x00 :type u16)
  (write-toggle t :type boolean))

(define-with-macro (ppu :conc-name nil)
  video-ram sprite-ram palette-ram
  sprite-address
  scroll-x scroll-y
  video-ram-address
  temporary-address
  write-toggle)

(defun read-pattern-tables (ppu address)
  (cartridge-read)
  )

(defun read-name-tables (ppu address)
  (TODO))

(defun read-palettes (ppu address)
  (aref (palette-ram ppu) (mirrored-address #x3F00 #x3F20 address :from-zero t)))

(defun pmref (ppu address)
  (setf addresss (mirrored-address #x0000 #x4000 address))
  (cond
    ((< address #x2000) (read-pattern-tables ppu address))
    ((< address #x3F00) (read-name-tables ppu address))
    ((< address #x4000) (read-palettes ppu address))))

(defun (setf pmref) (ppu address value)
  (setf addresss (mirrored-address #x0000 #x4000 address))
  (cond
    ((< address #x2000) (write-pattern-tables ppu address value))
    ((< address #x3F00) (write-name-tables ppu address value))
    ((< address #x4000) (write-palettes ppu address value))))

;;;; I/O Read -----------------------------------------------------------------
;;; http://wiki.nesdev.com/w/index.php/PPU_scrolling

(defun read-ppu/undefined (ppu)
  ;; todo: http://wiki.nesdev.com/w/index.php/PPU_registers#Ports
  (declare (ignore ppu))
  #x00)


(defun read-ppu/control (ppu)            ; 2000
  (read-ppu/undefined ppu))

(defun read-ppu/mask (ppu)               ; 2001
  (read-ppu/undefined ppu))

(defun read-ppu/status (ppu)             ; 2002
  (setf (write-toggle ppu) t)
  (TODO))

(defun read-ppu/sprite-ram-address (ppu) ; 2003
  (read-ppu/undefined ppu))

(defun read-ppu/sprite-ram-data (ppu)    ; 2004
  (TODO))

(defun read-ppu/scroll (ppu)             ; 2005
  (read-ppu/undefined ppu))

(defun read-ppu/address (ppu)            ; 2006
  (read-ppu/undefined ppu))

(defun read-ppu/data (ppu)               ; 2007
  (TODO))

(defun read-ppu/sprite-dma (ppu)         ; 4014
  (read-ppu/undefined ppu))


;;;; I/O Write ----------------------------------------------------------------
;;; http://wiki.nesdev.com/w/index.php/PPU_scrolling

(defun write-ppu/undefined (ppu value)
  ;; todo: http://wiki.nesdev.com/w/index.php/PPU_registers#Ports
  (declare (ignore ppu value))
  #x00)


(defun write-ppu/control (ppu value)            ; 2000
  (TODO))

(defun write-ppu/mask (ppu value)               ; 2001
  (TODO))

(defun write-ppu/status (ppu value)             ; 2002
  (write-ppu/undefined ppu value))

(defun write-ppu/sprite-ram-address (ppu value) ; 2003
  (TODO))

(defun write-ppu/sprite-ram-data (ppu value)    ; 2004
  (TODO))

(defun write-ppu/scroll (ppu value)             ; 2005
  (with-ppu (ppu)
    (if write-toggle
      (progn (TODO))
      (progn (TODO)))
    (notf write-toggle)))

(defun write-ppu/address (ppu value)            ; 2006
  (with-ppu (ppu)
    (if write-toggle
      (setf
        ;; t: .FEDCBA ........ = d: ..FEDCBA
        ;; t: X...... ........ = 0
        (ldb (byte 7 8) temporary-address) (ldb (byte 6 0) value))
      (setf
        ;; t: ....... HGFEDCBA = d: HGFEDCBA
        (ldb (byte 8 0) temporary-address) value
        ;; v                   = t
        video-ram-address temporary-address))
    (notf write-toggle)))

(defun write-ppu/data (ppu value)               ; 2007
  (TODO))

(defun write-ppu/sprite-dma (ppu value)         ; 4014
  (TODO))

