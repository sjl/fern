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

(defun check-redraw (gui)
  (let ((ts (fern::timestamp-pattern-tables (fern::ppu (nes gui)))))
    (if (> ts (timestamp gui))
      (progn (setf (timestamp gui) ts) t)
      nil)))


(define-shader textured ()
  :vertex "
  #version 330 core
  layout (location = 0) in vec3 inPosition;
  layout (location = 1) in vec2 inTextureCoord;

  out vec2 textureCoord;

  void main () {
      gl_Position = vec4(inPosition, 1.0);
      textureCoord = inTextureCoord;
  }
  "
  :fragment "
  #version 330 core

  in vec2 textureCoord;
  out vec4 FragColor;
  uniform sampler2D tex;

  void main () {
      FragColor = texture(tex, textureCoord);
  }
  ")


(defmethod initialize ((gui pattern-table-viewer))
  (setf (texture gui) (allocate-texture)
        (quad gui) (allocate-quad)
        (shader gui) (allocate-shader 'textured)))

(defmethod render ((gui pattern-table-viewer))
  (when (or (dirty gui)
            (check-redraw gui))
    (setf (dirty gui) nil)
    (refresh-pixels gui)
    (viewport (width gui) (height gui))
    (clear 0.2 0.2 0.2)
    (use-shader/textured (shader gui))
    (update-texture (texture gui) +w+ +w+ (pixels gui))
    (use-texture (texture gui))
    (draw-quad (quad gui))
    t))

(defmethod teardown ((gui pattern-table-viewer))
  (deallocate-texture (texture gui))
  (deallocate-quad (quad gui))
  (deallocate-shader (shader gui)))


;;;; API ----------------------------------------------------------------------
(defun open (&optional (nes fern::*nes*))
  (setf *pattern-table-viewer*
        (make-instance 'pattern-table-viewer
          :title "Pattern Tables"
          :nes nes
          :width 400
          :height 300))
  (fern/gui::open-gui *pattern-table-viewer*))
