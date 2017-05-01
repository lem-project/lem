(defsystem lem-lisp-mode
  :depends-on (:alexandria
               :trivial-types
               :usocket
               :swank
               :optima
               :uiop
               :lem-core)
  :serial t
  :components ((:file "errors")
               (:file "swank-protocol")
               (:file "button")
               (:file "package")
               (:file "lisp-ui-mode")
               (:file "lisp-mode")
               (:file "sldb")
               (:file "inspector")))
