(in-package :fern)

;;;; PPU ----------------------------------------------------------------------
(defstruct (ppu (:conc-name nil))
  (ppu-cartridge nil :type (or null cartridge))
  (palette-ram (make-memory #x20) :type (memory #x20))
  (sprite-ram (make-memory #x100) :type (memory #x100))
  (video-ram (make-memory #x1000) :type (memory #x1000))
  (sprite-ram-address #x00 :type u8)
  (scroll-x 0 :type u8)
  (scroll-y 0 :type u8)
  (video-ram-address #x00 :type u16)
  (video-ram-increment 1 :type (member 1 32))
  (temporary-address #x00 :type u16)
  (write-toggle t :type boolean))

(define-with-macro (ppu :conc-name nil)
  video-ram sprite-ram palette-ram
  scroll-x scroll-y
  sprite-ram-address
  video-ram-address
  temporary-address
  video-ram-increment
  write-toggle)


;;;; Video Memory -------------------------------------------------------------
(defun read-pattern-tables (ppu address)
  (check-type address (integer #x0000 (#x2000)))
  (cartridge-read-character (ppu-cartridge ppu) address))

(defun read-name-tables (ppu address)
  (check-type address (integer #x2000 (#x3F00)))
  (aref (video-ram ppu) (mirrored-address #x2000 #x3000 address :from-zero t)))

(defun read-palettes (ppu address)
  (check-type address (integer #x3F00 (#x4000)))
  (aref (palette-ram ppu) (mirrored-address #x3F00 #x3F20 address :from-zero t)))


(defun write-pattern-tables (ppu address value)
  (check-type address (integer #x0000 (#x2000)))
  (cartridge-write-character (ppu-cartridge ppu) address value))

(defun write-name-tables (ppu address value)
  (check-type address (integer #x2000 (#x3F00)))
  (setf (aref (video-ram ppu)
              (mirrored-address #x2000 #x3000 address :from-zero t))
        value))

(defun write-palettes (ppu address value)
  (check-type address (integer #x3F00 (#x4000)))
  (setf (aref (palette-ram ppu)
              (mirrored-address #x3F00 #x3F20 address :from-zero t))
        value))


(defun pref (ppu address)
  (setf address (mirrored-address #x0000 #x4000 address))
  (cond
    ((< address #x2000) (read-pattern-tables ppu address))
    ((< address #x3F00) (read-name-tables ppu address))
    ((< address #x4000) (read-palettes ppu address))))

(defun (setf pref) (new-value ppu address)
  (setf address (mirrored-address #x0000 #x4000 address))
  (cond
    ((< address #x2000) (write-pattern-tables ppu address new-value))
    ((< address #x3F00) (write-name-tables ppu address new-value))
    ((< address #x4000) (write-palettes ppu address new-value))))


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
  (aref (sprite-ram ppu) (sprite-ram-address ppu)))

(defun read-ppu/scroll (ppu)             ; 2005
  (read-ppu/undefined ppu))

(defun read-ppu/address (ppu)            ; 2006
  (read-ppu/undefined ppu))

(defun read-ppu/data (ppu)               ; 2007
  ;; todo handle the buffering stuff
  (with-ppu (ppu)
    (prog1 (pref ppu video-ram-address)
      (incf/16 video-ram-address video-ram-increment))))


(defun read-ppu (ppu address)            ; 2000-4000
  (ecase (mirrored-address #x2000 #x2008 address)
    (#x2000 (read-ppu/control ppu))
    (#x2001 (read-ppu/mask ppu))
    (#x2002 (read-ppu/status ppu))
    (#x2003 (read-ppu/sprite-ram-address ppu))
    (#x2004 (read-ppu/sprite-ram-data ppu))
    (#x2005 (read-ppu/scroll ppu))
    (#x2006 (read-ppu/address ppu))
    (#x2007 (read-ppu/data ppu))))


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
  (setf (sprite-ram-address ppu) value))

(defun write-ppu/sprite-ram-data (ppu value)    ; 2004
  (with-ppu (ppu)
    (setf (aref sprite-ram sprite-ram-address) value)
    (incf/8 sprite-ram-address)))

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
  (with-ppu (ppu)
    (setf (pref ppu video-ram-address) value)
    (incf/16 video-ram-address video-ram-increment)))


(defun write-ppu (ppu address value)            ; 2000-4000
  (ecase (mirrored-address #x2000 #x2008 address)
    (#x2000 (write-ppu/control ppu value))
    (#x2001 (write-ppu/mask ppu value))
    (#x2002 (write-ppu/status ppu value))
    (#x2003 (write-ppu/sprite-ram-address ppu value))
    (#x2004 (write-ppu/sprite-ram-data ppu value))
    (#x2005 (write-ppu/scroll ppu value))
    (#x2006 (write-ppu/address ppu value))
    (#x2007 (write-ppu/data ppu value))))

;;;; Pattern Tables / Tiles ----------------------------------------------------
(deftype pixel ()
  '(integer 0 3))

(deftype tile ()
  '(simple-array pixel (8 8)))


(defun tile-pixel (value)
  (aref " .x#" value))

(defun tile-string (tile)
  (with-output-to-string (s)
    (dotimes (row 8)
      (dotimes (col 8)
        (write-char (tile-pixel (aref tile row col)) s))
      (terpri s))))

(defun pixel (nes pattern-table tile-number row col)
  (flet ((pixel-at (base-address)
           (cat (ldbit (- 7 col) (pref (ppu nes) base-address)) ; low bitplane
                (ldbit (- 7 col) (pref (ppu nes) (+ base-address 8))) ; high bitplane
                1)))
    (-<> pattern-table
      (* <> #x1000)                    ; T???
      (logior <> (* tile-number #x10)) ; Tnn?
      (logior <> row)                  ; TnnR
      (pixel-at <>))))

(defun tile (nes pattern-table tile-number)
  (let ((result (make-array '(8 8) :element-type 'pixel)))
    (dotimes (row 8)
      (dotimes (col 8)
        (setf (aref result row col)
              (pixel nes pattern-table tile-number row col))))
    result))
