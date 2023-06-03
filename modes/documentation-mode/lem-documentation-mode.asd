(defsystem "lem-documentation-mode"
  :depends-on ("lem")
  :serial t
  :components ((:file "internal")
               (:file "documentation-mode")))
