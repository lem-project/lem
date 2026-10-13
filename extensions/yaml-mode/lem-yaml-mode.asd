(defsystem "lem-yaml-mode"
  :depends-on ("lem/core" "lem-tree-sitter" "lem-lsp-mode")
  :serial t
  :components ((:file "yaml-mode")
               (:file "lsp-config")))
