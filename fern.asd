(asdf:defsystem :fern
  :name "fern"
  :description "An NES emulator/workbench."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :alexandria
               :bordeaux-threads
               :cl-glfw3
               :cl-opengl
               :iterate
               :losh

               )

  :serial t
  :components ((:file "package")
               (:module "src" :serial t
                :components ((:file "utils")
                             (:module "core" :serial t
                              :components ((:file "cartridge")
                                           (:file "apu")
                                           (:file "ppu")
                                           (:file "nes")
                                           (:file "memory")
                                           (:file "addressing-modes")
                                           (:file "opcodes")
                                           (:file "disassemble")))
                             (:module "files" :serial nil
                              :components ((:file "ines")))
                             (:module "cartridges" :serial nil
                              :components ((:file "empty")
                                           (:file "000")))
                             (:module "gui" :serial t
                              :components ((:file "base")
                                           (:file "pattern-table-viewer")))
                             (:file "run")))))
