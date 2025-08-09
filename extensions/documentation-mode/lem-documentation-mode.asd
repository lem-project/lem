(defsystem "lem-documentation-mode"
  :depends-on ("lem/core" "lem-lisp-syntax")
  :serial t
  :components ((:file "utils")
	       (:file "internal")
	       (:file "documentation-mode")))
