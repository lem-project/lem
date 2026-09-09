(defsystem "lem-python-mode"
  :depends-on ("lem/core"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")
  :serial t
  :components ((:file "python-mode")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "run-python")
               (:file "lsp-config")))

(defsystem "lem-python-mode/call-graph"
  :description "Call graph provider for Python using tree-sitter"
  :depends-on ("lem-python-mode" "call-graph" "lem-tree-sitter")
  :components ((:file "call-graph-provider")))
