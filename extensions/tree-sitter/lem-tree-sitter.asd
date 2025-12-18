(defsystem "lem-tree-sitter"
  :version "0.1.0"
  :author "Lem Project"
  :license "MIT"
  :description "Tree-sitter integration for Lem editor"
  :depends-on ("lem/core" "cl-tree-sitter" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "highlight")
               (:file "integration")
               (:module "languages"
                :components ((:file "json")
                             (:file "markdown"))))
  :in-order-to ((test-op (test-op "lem-tree-sitter/tests"))))

(defsystem "lem-tree-sitter/tests"
  :depends-on ("lem-tree-sitter" "rove" "lem-fake-interface")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
