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
               (:file "lisp-mode")))
