(in-package :fern/gui/pattern-table-viewer)

;;;; Data ---------------------------------------------------------------------
(defconstant +w+ 256)
(defvar *pattern-table-viewer* nil)


;;;; Implementation -----------------------------------------------------------
(defclass* (pattern-table-viewer :conc-name nil) (gui)
  ((texture)
   (quad)
   (shader)
   (pixels :initform (make-array (* +w+ +w+ 3) :initial-element 100))
   (timestamp :initform -1 :type fixnum)))


(defun pixel-ref (pixels x y)
  (let* ((width (* +w+ 3))
         (index (+ (* 3 x) (* width (- (1- +w+) y)))))
    (values (aref pixels (+ index 0))
            (aref pixels (+ index 1))
            (aref pixels (+ index 2)))))

(define-setf-expander pixel-ref (pixels x y)
  ;; don't need to bother with the full get-setf-expander dance here because
  ;; we're updating a mutable array
  (alexandria:with-gensyms (pixels% i% r g b)
    (values `(,pixels% ,i%)
            `(,pixels (+ (* 3 ,x)
                         (* (* +w+ 3) (- (1- +w+) ,y))))
            `(,r ,g ,b)
            `(progn
               (setf (aref ,pixels% (+ ,i% 0)) ,r
                     (aref ,pixels% (+ ,i% 1)) ,g
                     (aref ,pixels% (+ ,i% 2)) ,b)
               (values ,r ,g ,b))
            `(values
               (aref ,pixels% (+ ,i% 0))
               (aref ,pixels% (+ ,i% 1))
               (aref ,pixels% (+ ,i% 2))))))


(defun refresh-pixels (gui)
  (let ((pixels (pixels gui))
        (nes (nes gui)))
    (do-range ((table 0 2)
               (tile 0 256)
               (row 0 8)
               (col 0 8))
      (setf (pixel-ref pixels
                       (+ (* 128 table)
                          (* 8 (mod tile 16))
                          col)
                       (+ (* 8 (truncate tile 16))
                          row))
            (case (fern::pixel nes table tile row col)
              (0 (values 0 0 0))
              (1 (values 255 100 0))
              (2 (values 50 255 50))
              (3 (values 0 100 255)))))))


(defmethod initialize ((gui pattern-table-viewer))
  (setf (texture gui) (allocate-texture)
        (quad gui) (allocate-quad)
        (shader gui) (allocate-shader 'textured)))

(defmethod teardown ((gui pattern-table-viewer))
  (deallocate-texture (texture gui))
  (deallocate-quad (quad gui))
  (deallocate-shader (shader gui)))

(defmethod dirtyp ((gui pattern-table-viewer))
  (/= (fern::timestamp-pattern-tables (fern::ppu (nes gui)))
      (timestamp gui)))

(defmethod render ((gui pattern-table-viewer))
  (setf (timestamp gui)
        (fern::timestamp-pattern-tables (fern::ppu (nes gui))))
  (refresh-pixels gui)
  (viewport (width gui) (height gui))
  (clear 0.2 0.2 0.2)
  (use-shader/textured (shader gui))
  (update-texture (texture gui) +w+ +w+ (pixels gui))
  (use-texture (texture gui))
  (draw-quad (quad gui)))


;;;; API ----------------------------------------------------------------------
(defun open (&optional (nes fern::*nes*))
  (setf *pattern-table-viewer*
        (make-instance 'pattern-table-viewer
          :title "Pattern Tables"
          :nes nes
          :width 400
          :height 300))
  (fern/gui:open *pattern-table-viewer*))
