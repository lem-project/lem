(defsystem "lem-python-mode"
  :depends-on ("lem/core"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "python-mode")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "run-python")))
