(defsystem "tree-sitter-cl"
  :version "0.1.0"
  :author "Lem Project"
  :license "MIT"
  :description "Common Lisp bindings for tree-sitter"
  :depends-on ("cffi" "alexandria" "trivial-garbage" "babel")
  :serial t
  :components ((:file "package")
               (:file "ffi")
               (:file "types")
               (:file "parser")
               (:file "node")
               (:file "query")
               (:file "language"))
  :in-order-to ((test-op (test-op "tree-sitter-cl/tests"))))

(defsystem "tree-sitter-cl/tests"
  :depends-on ("tree-sitter-cl" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
