(defsystem "lem-python-mode"
  :depends-on ("lem-core"
               #+#.(cl:if (ql:where-is-system :async-process) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "python-mode")
               #+#.(cl:if (ql:where-is-system :async-process) '(and) '(or))
               (:file "run-python")))
