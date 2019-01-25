(in-package :fern/gui)

;;;; Shaders ------------------------------------------------------------------
(defun compile-shader (handle source)
  (gl:shader-source handle source)
  (gl:compile-shader handle)
  (if (gl:get-shader handle :compile-status)
    handle
    (error "Shader compilation error:~2%~A~%" (gl:get-shader-info-log handle))))

(defun allocate-and-compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (compile-shader shader source)
    shader))

(defun make-program (vertex-shader fragment-shader)
  (let ((program (gl:create-program)))
    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)
    (if (gl:get-program program :link-status)
      program
      (let ((log (gl:get-program-info-log program)))
        (gl:delete-program program)
        (error "Shader link error:~2%~A~%" log)))))

(defun compile-shaders-into-program (vertex fragment)
  (let ((vertex-shader (allocate-and-compile-shader :vertex-shader vertex)))
    (unwind-protect
        (let ((fragment-shader (allocate-and-compile-shader :fragment-shader fragment)))
          (unwind-protect
              (make-program vertex-shader fragment-shader)
            (gl:delete-shader fragment-shader)))
      (gl:delete-shader vertex-shader))))


(defun set-uniform (location type value)
  (ecase type
    (vec4 (gl:uniformfv location value))
    (float (gl:uniformf location value))
    (texture (gl:uniformi location value))))


(defclass* (shader :conc-name nil) ()
  ((name)
   (tag :initform nil)
   (program :initform nil)))

(defgeneric vertex (shader))
(defgeneric fragment (shader))


(defun allocate-shader (name)
  (make-instance name :name name))

(defun recompile-shader (shader)
  ;; (format t "Recompiling ~A~%" shader)
  (when (program shader)
    (gl:delete-program (program shader)))
  (setf (tag shader) (current-tag shader)
        (program shader) (compile-shaders-into-program (vertex shader)
                                                       (fragment shader)))
  (values))

(defun needs-recompile-p (shader)
  (not (eql (tag shader) (current-tag shader))))

(defun current-tag (shader)
  (get (name shader) 'current-tag))

(defun deallocate-shader (shader)
  (when (program shader)
    (gl:delete-program (program shader))))


(defmacro define-shader (name uniforms &key vertex fragment)
  (let ((tag (gensym))
        (use-shader (alexandria:symbolicate 'use-shader/ name)))
    `(progn
       (defclass ,name (shader) ())
       (setf (get ',name 'current-tag) ',tag)
       (defmethod vertex ((shader ,name)) ,vertex)
       (defmethod fragment ((shader ,name)) ,fragment)
       (defun ,use-shader (shader ,@(mapcar #'car uniforms))
         (when (needs-recompile-p shader)
           (recompile-shader shader))
         (let ((program (program shader)))
           (gl:use-program program)
           ,@(loop
               :for (symbol type) :in uniforms
               :collect `(set-uniform
                           (gl:get-uniform-location
                             program
                             ,(cffi:translate-camelcase-name symbol))
                           ',type
                           ,symbol)))))))


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


;;;; OpenGL Boilerplate -------------------------------------------------------
(defun allocate-opengl-array (lisp-array type)
  (let ((opengl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref opengl-array i) (aref lisp-array i)))
    opengl-array))

(defmacro with-opengl-array ((symbol lisp-array type) &body body)
  `(let ((,symbol (allocate-opengl-array ,lisp-array ,type)))
     (unwind-protect
         (progn ,@body)
       (gl:free-gl-array ,symbol))))


(defun floats (&optional (n 1))
  (* n (cffi:foreign-type-size :float)))

(defun clear (r g b &optional (a 1.0))
  (gl:clear-color r g b a)
  (gl:clear :color-buffer))

(defun viewport (width height)
  (gl:viewport 0 0 width height))


;;;; Base GUI -----------------------------------------------------------------
(defclass* (gui :conc-name nil) ()
  ((nes :type fern::nes)
   (title)
   (window)
   (width)
   (height)
   (dirty :initform t)))


(defgeneric initialize (gui))
(defgeneric dirtyp (gui))
(defgeneric render (gui))
(defgeneric teardown (gui))

(defmethod dirtyp ((gui gui)) nil)


(defparameter *window-guis*
  (make-hash-table :test #'cffi:pointer-eq
                   :hash-function (lambda (pointer)
                                    (sxhash (cffi:pointer-address pointer)))))

(defun window-gui (window)
  (gethash window *window-guis*))

(defmacro with-gui-window-mapping ((gui window) &body body)
  (alexandria:once-only (gui window)
    `(unwind-protect (progn
                       (setf (gethash ,window *window-guis*) ,gui)
                       ,@body)
       (remhash ,window *window-guis*))))


;;;; GLFW Boilerplate ---------------------------------------------------------
;;; Resize
(defgeneric handle-resize (gui width height))

(defmethod handle-resize ((gui gui) width height)
  nil)

(defmethod handle-resize :before ((gui gui) width height)
  (setf (dirty gui) t
        (width gui) width
        (height gui) height))

(glfw:def-window-size-callback handle-resize-callback (window width height)
  (handle-resize (window-gui window) width height))


;;; Keyboard
(defgeneric handle-key (gui key scancode action mod-keys))

(defmethod handle-key ((gui gui) key scancode action mod-keys)
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close (window gui))))

(glfw:def-key-callback handle-key-callback (window key scancode action mod-keys)
  (handle-key (window-gui window) key scancode action mod-keys))


;;; Refresh
(defgeneric handle-refresh (gui))

(defmethod handle-refresh ((gui gui))
  nil)

(defmethod handle-refresh :before ((gui gui))
  (setf (dirty gui) t))

(glfw:def-window-refresh-callback handle-refresh-callback (window)
  (handle-refresh (window-gui window)))


;;;; Quads --------------------------------------------------------------------
(defclass* (quad :conc-name nil) ()
  ((vertex-buffer)
   (index-buffer)
   (vertex-array)))

(defun allocate-quad ()
  (let ((vertex-buffer (gl:gen-buffer))
        (index-buffer (gl:gen-buffer))
        (vertex-array (gl:gen-vertex-array))
        ;;          position         texture coords
        (vertexes #(-1.0  1.0  0.0   0.0 1.0
                    -1.0 -1.0  0.0   0.0 0.0
                    1.0 -1.0  0.0   1.0 0.0
                    1.0  1.0  0.0   1.0 1.0))
        (indexes #(0 1 2
                   0 2 3)))
    (gl:bind-vertex-array vertex-array)

    (gl:bind-buffer :array-buffer vertex-buffer)
    (with-opengl-array (array vertexes :float)
      (gl:buffer-data :array-buffer :static-draw array))

    (gl:bind-buffer :element-array-buffer index-buffer)
    (with-opengl-array (array indexes :unsigned-int)
      (gl:buffer-data :element-array-buffer :static-draw array))

    ;; position
    (gl:vertex-attrib-pointer 0 3 :float nil (floats 5) 0)
    (gl:enable-vertex-attrib-array 0)

    ;; texture coordinates
    (gl:vertex-attrib-pointer 1 2 :float nil (floats 5) (floats 3))
    (gl:enable-vertex-attrib-array 1)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)

    (make-instance 'quad
      :index-buffer index-buffer
      :vertex-buffer vertex-buffer
      :vertex-array vertex-array)))

(defun draw-quad (quad)
  (gl:bind-vertex-array (vertex-array quad))
  (%gl:draw-elements :triangles 6 :unsigned-int 0))

(defun deallocate-quad (quad)
  (gl:delete-vertex-arrays (list (vertex-array quad)))
  (gl:delete-buffers (list (index-buffer quad) (vertex-buffer quad))))


;;;; Textures -----------------------------------------------------------------
(defun allocate-texture (&optional (texture-unit 0))
  (let* ((texture (gl:gen-texture)))
    (gl:active-texture texture-unit)
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    texture))

(defun deallocate-texture (texture)
  (gl:delete-texture texture))

(defun update-texture (texture width height data)
  (gl:bind-texture :texture-2d texture)
  (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
  (gl:generate-mipmap :texture-2d)
  (values))

(defun use-texture (texture)
  (gl:bind-texture :texture-2d texture))


;;;; Main Loop ----------------------------------------------------------------
(defvar *new-guis* nil) ; TODO thread safety on this thing
(defvar *guis* nil)
(defvar *gui-thread* nil)
(defparameter *running* nil)

(defun add-new-gui (gui)
  (glfw:create-window :title (title gui)
                      :width (width gui)
                      :height (height gui)
                      :context-version-major 3
                      :context-version-minor 3
                      :opengl-profile :opengl-core-profile)
  (setf (window gui) glfw:*window*
        (gethash glfw:*window* *window-guis*) gui)
  (initialize gui)
  (glfw:set-key-callback 'handle-key-callback)
  (glfw:set-window-size-callback 'handle-resize-callback)
  (glfw:set-window-refresh-callback 'handle-refresh-callback)
  (push gui *guis*))

(defun add-new-guis ()
  (map nil #'add-new-gui *new-guis*)
  (setf *new-guis* nil))

(defun render-gui (gui)
  (glfw:make-context-current (window gui))
  (if (glfw:window-should-close-p)
    (progn (teardown gui)
           (glfw:destroy-window)
           (alexandria:removef *guis* gui))
    (when (or (dirty gui) (dirtyp gui))
      (setf (dirty gui) nil)
      (render gui)
      (glfw:swap-buffers))))

(defun render-guis ()
  (map nil #'render-gui *guis*))

(defun gui-loop% ()
  (setf *running* t)
  (clrhash *window-guis*)
  (glfw:with-init
    (unwind-protect
        (iterate
          (timing real-time :per-iteration-into loop-time)
          (pr 'time (* loop-time internal-time-units-per-second))
          (while *running*)
          (add-new-guis)
          (render-guis)
          (glfw:poll-events))
      (map nil #'teardown *guis*)
      (setf *guis* nil
            *new-guis* nil))))

(defun gui-loop (&key background)
  (setf *running* nil)
  (sleep 1/20) ; close enough
  (if background
    (setf *gui-thread* (bt:make-thread #'gui-loop% :name "Fern GUI thread"))
    (gui-loop%)))

(defun open-gui (gui)
  (push gui *new-guis*))


