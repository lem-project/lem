(defsystem "lem-typescript-mode"
  :depends-on ("lem/core" "lem-lsp-mode" "lem-js-mode" "lem-tree-sitter")
  :serial t
  :components ((:file "typescript-mode")
               (:file "lsp-config")))
