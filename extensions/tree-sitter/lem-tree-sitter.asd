(defsystem "lem-tree-sitter"
  :version "0.1.0"
  :author "Lem Project"
  :license "MIT"
  :description "Tree-sitter integration for Lem editor"
  :depends-on ("lem/core" "tree-sitter-cl" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "highlight")
               (:file "indent")
               (:file "integration")))
