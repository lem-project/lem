(defsystem "lem-c-mode"
  :depends-on ("lem" "lem-lisp-mode")
  :serial t
  :components ((:file "grammer")
               (:file "c-mode")))
