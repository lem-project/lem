(defsystem "lem-lisp-syntax"
  :depends-on ("lem-base" "cl-ppcre")
  :pathname "lem-lisp-syntax/"
  :serial t
  :components ((:file "indent")
               (:file "syntax-table")
               (:file "enclosing")))
