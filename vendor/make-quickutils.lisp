(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :symb
               :with-gensyms

               )
  :package "PCG.QUICKUTILS")
