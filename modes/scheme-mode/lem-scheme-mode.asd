(defsystem "lem-scheme-mode"
  :depends-on ("uiop"
               #+(or)"lem-process"
               "lem-core")
  :serial t
  :components ((:file "syntax-indent")
               (:file "syntax-syntax-table")
               (:file "syntax-misc")
               (:file "lem-scheme-syntax")
               (:file "package")
               (:file "grammer")
               (:file "scheme-mode")
               #+(or)(:file "eval")))
