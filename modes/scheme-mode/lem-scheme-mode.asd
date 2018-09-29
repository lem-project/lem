(defsystem "lem-scheme-mode"
  :depends-on ("uiop"
               #+#.(cl:if (cl:find-package :async-process) '(and) '(or)) "lem-process"
               "lem-core")
  :serial t
  :components ((:file "syntax-indent")
               (:file "syntax-syntax-table")
               (:file "syntax-misc")
               (:file "lem-scheme-syntax")
               (:file "package")
               (:file "grammer")
               (:file "scheme-mode")
               #+#.(cl:if (cl:find-package :async-process) '(and) '(or))
               (:file "eval")))
