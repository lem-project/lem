(defsystem "lem-documentation-mode"
  :depends-on ("lem")
  :serial t
  :components ((:file "utils")
	       (:file "internal")
	       (:file "documentation-mode")))
