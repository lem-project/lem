(defsystem "lem-json-mode"
  :depends-on ("lem/core" "lem-js-mode" "lem-tree-sitter")
  :serial t
  :components ((:file "json-mode")))
