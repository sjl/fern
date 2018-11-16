(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-gethash
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "FERN.QUICKUTILS")
