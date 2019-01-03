(asdf:defsystem :fern
  :name "fern"
  :description "An NES emulator/workbench."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :alexandria
               :iterate
               :losh

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "utils")
                             (:file "main")
                             (:module "files" :serial nil
                              :components ((:file "ines")))
                             (:module "cartridges" :serial nil
                              :components ((:file "empty")
                                           (:file "000")))))))
