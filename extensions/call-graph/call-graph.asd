(defsystem "call-graph"
  :description "Language-agnostic call graph data structures and provider protocol"
  :author "Lem Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("alexandria" "yason")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "provider")
               (:file "json-format"))
  :in-order-to ((test-op (test-op "call-graph/tests"))))

(defsystem "call-graph/tests"
  :depends-on ("call-graph" "rove")
  :pathname "tests"
  :components ((:file "types-test")
               (:file "provider-test")
               (:file "json-format-test"))
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
