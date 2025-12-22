(defsystem "lem-go-mode"
  :depends-on ("lem/core" "yason" "lem-lsp-mode")
  :serial t
  :components ((:file "go-mode")
               (:file "lsp-config")))

(defsystem "lem-go-mode/call-graph"
  :description "Call graph provider for Go using tree-sitter"
  :depends-on ("lem-go-mode" "call-graph" "lem-tree-sitter")
  :components ((:file "call-graph-provider")))
