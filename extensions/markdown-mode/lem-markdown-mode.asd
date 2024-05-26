(defsystem "lem-markdown-mode"
  :depends-on ("lem")
  :serial t
  :components ((:file "syntax-parser")
               (:file "markdown-mode")))
