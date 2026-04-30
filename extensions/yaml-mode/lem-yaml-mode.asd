(defsystem "lem-yaml-mode"
  :depends-on ("lem/core" "lem-tree-sitter")
  :serial t
  :components ((:file "yaml-mode")))
