(defsystem "lem-js-mode"
  :depends-on ("lem/core"
               "lem-lsp-mode"
               "lem-xml-mode")
  :serial t
  :components ((:file "js-mode")
               (:file "eslint")
               (:file "lsp-config")))

(defsystem "lem-js-mode/call-graph"
  :description "Call graph provider for JavaScript/TypeScript using tree-sitter"
  :depends-on ("lem-js-mode" "call-graph" "lem-tree-sitter")
  :components ((:file "call-graph-provider")))

