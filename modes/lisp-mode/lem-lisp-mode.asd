(defsystem "lem-lisp-mode"
  :author "cxxxr"
  :license "MIT"
  :description "lisp mode for the Lem editor"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "swank"
               "optima"
               "uiop"
               "lem-core")
  :serial t
  :components ((:file "errors")
               (:file "swank-protocol")
               (:file "package")
               (:file "lisp-ui-mode")
               (:file "grammer")
               (:file "lisp-mode")
               (:file "repl")
               (:file "sldb")
               (:file "inspector")
               (:file "apropos-mode")
               (:file "paren-coloring")))
