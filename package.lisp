(defpackage :fern
  (:use :cl :iterate :losh)
  (:export))

(defpackage :fern/gui
  (:use :cl :iterate :losh :fern)
  (:shadow :open)
  (:export
    :gui
    :title
    :width
    :height
    :dirty
    :nes
    :window

    :open
    :main-loop

    :textured
    :use-shader/textured

    :initialize
    :render
    :teardown
    :dirtyp

    :handle-resize
    :handle-key
    :handle-refresh

    :define-shader
    :allocate-shader
    :deallocate-shader

    :clear
    :viewport

    :quad
    :allocate-quad
    :deallocate-quad
    :draw-quad

    :allocate-texture
    :deallocate-texture
    :update-texture
    :use-texture))

(defpackage :fern/gui/pattern-table-viewer
  (:use :cl :iterate :losh :fern :fern/gui)
  (:shadow
    :open)
  (:export
    :open))

(defpackage :fern/gui/name-table-viewer
  (:use :cl :iterate :losh :fern :fern/gui)
  (:shadow
   :open)
  (:export
    :open))
