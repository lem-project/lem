(defsystem "lem-base"
  :depends-on ("uiop"
               "iterate"
               "alexandria"
               "cl-ppcre"
               "cl-annot")
  :serial t
  :components ((:file "package")
               (:file "documentation")
               (:file "util")
               (:file "errors")
               (:file "var")
               (:file "wide")
               (:file "macros")
               (:file "hooks")
               (:file "line")
               (:file "buffer")
               (:file "buffer-insert")
               (:file "buffers")
               (:file "point")
               (:file "basic")
               (:file "syntax")
               (:file "syntax-parser")
               (:file "tmlanguage")
               (:file "file")
               (:file "search")
               (:file "indent")))
