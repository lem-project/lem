(defsystem "lem-markdown-mode"
  :depends-on ("lem"
               "3bmd"
               "3bmd-ext-code-blocks"
               "lisp-preprocessor"
               "trivial-ws"
               "trivial-open-browser")
  :serial t
  :components ((:file "syntax-parser")
               (:file "markdown-mode")
               (:file "preview")))
