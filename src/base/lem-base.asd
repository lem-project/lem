(defsystem "lem-base"
  :depends-on ("iterate"
               "alexandria"
               "cl-ppcre"
               "babel"
               "log4cl"
               "closer-mop"
               "trivia")
  :serial t
  :components ((:file "utils")
               (:file "icon")
               (:file "eastasian")
               (:file "string-width-utils")
               (:file "errors")
               (:file "hooks")
               (:file "file-utils")
               (:file "line")
               (:file "package")
               (:file "buffer")
               (:file "var")
               (:file "editor-variables")
               (:file "macros")
               (:file "edit")
               (:file "mark")
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
               (:file "indent")
               (:file "check-corruption")))
