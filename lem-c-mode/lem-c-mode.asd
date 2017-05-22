(defsystem "lem-c-mode"
  :depends-on ("lem-core")
  :serial t
  :components ((:file "grammer")
               (:file "c-mode")))
