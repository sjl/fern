(in-package :fern/gui/name-table-viewer)

;;;; Data ---------------------------------------------------------------------
(defconstant +w+ 512)
(defvar *name-table-viewer* nil)


;;;; Implementation -----------------------------------------------------------
(defclass* (name-table-viewer :conc-name nil) (gui)
  ((texture)
   (quad)
   (shader)
   (pixels :initform (make-array (* +w+ +w+ 3) :initial-element 0))
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
  (let ((pixels (pixels gui)))
    ;; top left
    (do-range ((row 0 240)
               (col 0 256))
      (setf (pixel-ref pixels col row) (values 100 0 50)))
    ;; top right
    (do-range ((row 0 240)
               (col 256 512))
      (setf (pixel-ref pixels col row) (values 0 100 50)))
    ;; bottom left
    (do-range ((row 240 480)
               (col 0 256))
      (setf (pixel-ref pixels col row) (values 50 100 0)))
    ;; bottom right
    (do-range ((row 240 480)
               (col 256 512))
      (setf (pixel-ref pixels col row) (values 0 50 100)))))


(defmethod initialize ((gui name-table-viewer))
  (setf (texture gui) (allocate-texture)
        (quad gui) (allocate-quad)
        (shader gui) (allocate-shader 'textured)))

(defmethod teardown ((gui name-table-viewer))
  (deallocate-texture (texture gui))
  (deallocate-quad (quad gui))
  (deallocate-shader (shader gui)))

(defmethod dirtyp ((gui name-table-viewer))
  (let ((ts (fern::timestamp-name-tables (fern::ppu (nes gui)))))
    (if (> ts (timestamp gui))
      (progn (setf (timestamp gui) ts) t)
      nil)))

(defmethod render ((gui name-table-viewer))
  (refresh-pixels gui)
  (viewport (width gui) (height gui))
  (clear 0.2 0.2 0.2)
  (use-shader/textured (shader gui))
  (update-texture (texture gui) +w+ +w+ (pixels gui))
  (use-texture (texture gui))
  (draw-quad (quad gui)))


;;;; API ----------------------------------------------------------------------
(defun open (&optional (nes fern::*nes*))
  (setf *name-table-viewer*
        (make-instance 'name-table-viewer
          :title "Name Tables"
          :nes nes
          :width 400
          :height 300))
  (fern/gui::open-gui *name-table-viewer*))
