(defsystem "lem-lisp-syntax"
  :depends-on ("lem-base" "cl-ppcre")
  :serial t
  :components ((:file "indent")
               (:file "syntax-table")
               (:file "enclosing")))
