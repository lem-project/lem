(defsystem "lem-lisp-syntax"
  :author "cxxxr"
  :license "MIT"
  :description "lisp syntax and indentation for the Lem editor"
  :depends-on ("lem-base" "cl-ppcre" "swank")
  :serial t
  :components ((:file "indent")
               (:file "syntax-table")
               (:file "misc")
               (:file "enclosing")
               (:file "parse")
               (:file "lem-lisp-syntax")))
