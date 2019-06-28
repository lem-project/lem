(defsystem "lem-scheme-mode"
  :depends-on ("alexandria"
               "uiop"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process"
               "lem-core")
  :serial t
  :components ((:file "syntax-data")
               (:file "syntax-indent")
               (:file "syntax-syntax-table")
               (:file "syntax-misc")
               (:file "lem-scheme-syntax")
               (:file "package")
               (:file "grammer")
               (:file "scheme-mode")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "eval")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "repl")))
