(defsystem "lem-base"
  :depends-on ("uiop"
               "iterate"
               "alexandria"
               "cl-ppcre"
               "babel")
  :serial t
  :components ((:file "string-width-utils")
               (:file "file-utils")
               (:file "utils")
               (:file "errors")
               (:file "hooks")
               (:file "package")
               (:file "var")
               (:file "editor-variables")
               (:file "macros")
               (:file "line")
               (:file "edit")
               (:file "buffer")
               (:file "buffer-insert")
               (:file "buffer-list-manager")
               (:file "buffers")
               (:file "point")
               (:file "basic")
               (:file "syntax-table")
               (:file "search")
               (:file "parse-partial-sexp")
               (:file "syntax-scan")
               (:file "syntax-parser")
               (:file "tmlanguage")
               (:file "encodings")
               (:file "file")
               (:file "indent")))
