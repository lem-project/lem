(defsystem "lem-living-canvas"
  :description "Living Canvas - Visual function graph editor for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("call-graph"
               "alexandria"
               "lem/core"
               "lem-lisp-mode"
               "lem-tree-sitter"
               "yason")
  :serial t
  :components ((:file "language-detection")
               (:file "micros-cl-provider")
               (:file "python-provider")
               (:file "js-provider")
               (:file "go-provider")
               (:file "buffer")
               (:file "living-canvas"))
  :in-order-to ((test-op (test-op "lem-living-canvas/tests"))))

(defsystem "lem-living-canvas/tests"
  :description "Tests for Living Canvas"
  :depends-on ("lem-living-canvas" "rove")
  :pathname "tests"
  :components ((:file "language-detection-test")
               (:file "python-provider-test")
               (:file "js-provider-test")
               (:file "micros-cl-provider-test"))
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
