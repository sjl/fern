(defpackage :fern
  (:use :cl :iterate :losh)
  (:export))

(defpackage :fern/gui
  (:use :cl :iterate :losh :fern)
  (:export
    :gui
    :width
    :height
    :dirty
    :nes
    :window
    :with-gui

    :initialize
    :render
    :teardown

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
