(defsystem "cl-tree-sitter"
  :version "0.1.0"
  :author "Lem Project"
  :license "MIT"
  :description "Common Lisp bindings for tree-sitter"
  :depends-on ("cffi" "alexandria" "trivial-garbage")
  :serial t
  :components ((:file "package")
               (:file "ffi")
               (:file "types")
               (:file "parser")
               (:file "node")
               (:file "query")
               (:file "language"))
  :in-order-to ((test-op (test-op "cl-tree-sitter/tests"))))

(defsystem "cl-tree-sitter/tests"
  :depends-on ("cl-tree-sitter" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
