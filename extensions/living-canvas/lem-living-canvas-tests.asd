(defsystem "lem-living-canvas-tests"
  :description "Tests for Living Canvas data layer"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("lem-living-canvas" "rove")
  :components ((:module "tests"
                :components ((:file "data-layer-test"))))
  :perform (test-op (op c)
                    (symbol-call :rove '#:run c)))
