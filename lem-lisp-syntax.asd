(defsystem "lem-lisp-syntax"
  :depends-on ("lem-base" "cl-ppcre")
  :pathname "lem-lisp-syntax/"
  :serial t
  :components ((:file "lisp-indent")))
