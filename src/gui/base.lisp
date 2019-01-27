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


;;;; GLFW Callback Boilerplate ------------------------------------------------
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
(defparameter *running* nil
  "Global variable that controls when the main GUI loop should be running.")

(defvar *running-guis* nil
  "List of currently-running GUI objects.

  This list is controlled entirely by the main GUI thread — it must not be
  accessed from any other thread.")

(defvar *main-gui-thread* nil
  "A handle to the main GUI thread, if running in the background.")

(defvar *new-guis*
  (make-instance 'jpl-queues:synchronized-queue
    :queue (make-instance 'jpl-queues:unbounded-fifo-queue))
  "A queue of new GUI objects for the main thread to render.

  This allows user threads to pass new GUI object to the main thread for opening
  via `open`.")


(defun destroy-gui (gui)
  "Destroy `gui`.

  Must be called from the main GUI thread.

  This will run `teardown`, remove it from the window/GUI hash table, and
  destroy its underlying GLFW window.

  Errors will be logged and ignored.

  "
  (handler-case
      (unwind-protect
          (progn (remhash (window gui) *window-guis*)
                 (teardown gui))
        (glfw:destroy-window (window gui)))
    (error (c) (warn "Error tearing down GUI ~A: ~A" gui c))))

(defun create-glfw-window-for-gui (gui)
  "Create a GLFW window for `gui`.

  Must be run from the main GUI thread.

  "
  (glfw:create-window :title (title gui)
                      :width (width gui)
                      :height (height gui)
                      :context-version-major 3
                      :context-version-minor 3
                      :opengl-profile :opengl-core-profile)
  (setf (window gui) glfw:*window*
        (gethash glfw:*window* *window-guis*) gui)
  (glfw:set-key-callback 'handle-key-callback)
  (glfw:set-window-size-callback 'handle-resize-callback)
  (glfw:set-window-refresh-callback 'handle-refresh-callback))

(defun render-gui (gui)
  "Render `gui` if necessary, returning whether a render happened.

  Must be run from the main GUI thread.

  `t` will be returned if a render happened (i.e. if this GUI needs its window's
  buffer swapped this frame), `nil` otherwise.

  "
  (if (or (dirty gui) (dirtyp gui))
    (progn
      (setf (dirty gui) nil)
      (glfw:make-context-current (window gui))
      (render gui)
      t)
    nil))

(defun render-guis ()
  "Render currently running GUIs and return a list of GUIs needing buffer swaps.

  Must be run from the main GUI thread.

  Any GUIs that are ready to close will be removed from the list of currently
  running GUIs and disposed of.

  "
  (iterate
    (for gui :in *running-guis*)
    (for window = (window gui))
    (if (glfw:window-should-close-p window)
      (progn
        (alexandria:removef *running-guis* gui)
        (destroy-gui gui))
      (when (render-gui gui)
        (collect gui)))))

(defun swap-buffers (guis)
  "Swap buffers for each gui in `guis`, waiting for vsync.

  Must be run from the main GUI thread.

  If `guis` is null, `sleep` will be called to avoid spin looping.

  "
  (if (null guis)
    (sleep 1/100)
    (progn
      (glfw:make-context-current (window (first guis)))
      (glfw:swap-interval 1)
      (glfw:swap-buffers)
      (glfw:swap-interval 0)
      (dolist (gui (rest guis))
        (glfw:make-context-current (window gui))
        (glfw:swap-buffers)))))

(defun add-new-guis ()
  "Initialize and add any newly queued guis.

  Must be run from the main GUI thread.

  "
  (iterate
    (until (jpl-queues:empty? *new-guis*))
    (for gui = (jpl-queues:dequeue *new-guis*))
    (create-glfw-window-for-gui gui)
    (initialize gui)
    (push gui *running-guis*)))

(defun main-loop% ()
  (setf *running* t
        *running-guis* nil)
  (glfw:with-init
    (unwind-protect
        (iterate
          (while *running*)
          (add-new-guis)
          (swap-buffers (render-guis))
          (glfw:poll-events)
          (sleep 1/100))
      (map nil #'destroy-gui *running-guis*))))

(defun main-loop (&key background)
  "Start the main GUI loop.

  If `background` is true, the loop will be run in a new thread will be spun up
  in the background to avoid blocking the REPL.  Set `*running*` to `nil` to
  stop it.

  If a main GUI loop is already running, it will be (gracefully) stopped first.

  TODO: Force main loop on MacOS.

  "
  (setf *running* nil)
  (sleep 1) ; close enough
  (if background
    (setf *main-gui-thread*
          (bt:make-thread #'main-loop% :name "Fern GUI Main Loop"))
    (main-loop%)))

(defun open (gui)
  "Open `gui`.

  This function can be called from any thread.

  "
  (jpl-queues:enqueue gui *new-guis*))


