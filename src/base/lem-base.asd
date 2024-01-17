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
               (:file "buffer-list-manager")
               (:file "syntax-table")
               (:file "var")
               (:file "interrupt")

               (:file "package")
               (:file "var-buffer")
               (:file "editor-variables")
               (:file "buffer")
               (:file "point")
               (:file "edit")
               (:file "mark")
               (:file "buffer-insert")
               (:file "basic")
               (:file "syntax-predicates")
               (:file "search")
               (:file "parse-partial-sexp")
               (:file "syntax-scan")
               (:file "syntax-parser")
               (:file "tmlanguage")
               (:file "check-corruption")

               (:file "encodings")
               (:file "file")
               (:file "indent")))
