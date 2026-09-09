(defsystem "lem-c-mode"
  :depends-on ("lem/core" "lem-lisp-mode")
  :serial t
  :components ((:file "grammar")
               (:file "format")
               (:file "c-mode")))
