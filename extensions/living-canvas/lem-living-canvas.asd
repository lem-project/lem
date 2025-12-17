(defsystem "lem-living-canvas"
  :description "Living Canvas - Visual function graph editor for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("call-graph"
               "alexandria"
               "lem/core"
               "lem-lisp-mode"
               "yason")
  :serial t
  :components ((:file "micros-cl-provider")
               (:file "buffer")
               (:file "living-canvas"))
  :in-order-to ((test-op (test-op "lem-living-canvas/tests"))))

(defsystem "lem-living-canvas/tests"
  :description "Tests for Living Canvas"
  :depends-on ("lem-living-canvas" "rove")
  :pathname "tests"
  :components ((:file "micros-cl-provider-test"))
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
