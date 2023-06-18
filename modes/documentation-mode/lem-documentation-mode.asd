(defsystem "lem-documentation-mode"
  :depends-on ("lem" "rove")
  :serial t
  :components ((:file "utils")
               (:file "internal")
               (:file "documentation-mode")
	       (:file "documentation-tests")))
