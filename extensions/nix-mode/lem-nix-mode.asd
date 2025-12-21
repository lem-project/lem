(defsystem "lem-nix-mode"
  :depends-on ("lem/core" "lem-tree-sitter" "lem-lsp-mode")
  :serial t
  :components ((:file "indent")
               (:file "nix-mode")
               (:file "lsp-config")
               (:module "tree-sitter"
                :components ((:static-file "highlights.scm")))))
