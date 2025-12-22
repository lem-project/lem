(defsystem "call-graph-lsp"
  :description "LSP Call Hierarchy to call-graph conversion (Lem-independent)"
  :author "Lem Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("call-graph" "lem-lsp-base")
  :serial t
  :components ((:file "package")
               (:file "lsp-converter")
               (:file "collector"))
  :in-order-to ((test-op (test-op "call-graph-lsp/tests"))))

(defsystem "call-graph-lsp/tests"
  :description "Tests for call-graph-lsp"
  :depends-on ("call-graph-lsp" "rove")
  :pathname "tests"
  :components ((:file "lsp-converter-test")
               (:file "collector-test"))
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
