(defsystem "lem-lisp-mode"
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
               (:file "sldb")
               (:file "inspector")
               (:file "apropos-mode")
               (:file "lisp-definitions")
               (:file "paren-coloring")))
