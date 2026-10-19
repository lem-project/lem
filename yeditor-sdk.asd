;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-sdk"
  :description "Reversible component lifecycle for Yeditor"
  :license "MIT"
  :depends-on ("yeditor-context")
  :components ((:file "yeditor/sdk"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-sdk/tests"))))

(asdf:defsystem "yeditor-sdk/tests"
  :depends-on ("yeditor-sdk")
  :components ((:file "yeditor/tests/sdk-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :yeditor-sdk/tests :run-tests)))

(asdf:defsystem "yeditor-sdk/rove-tests"
  :depends-on ("yeditor-sdk/tests" "rove")
  :components ((:file "yeditor/tests/sdk-rove-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op))
             (unless (uiop:symbol-call :rove :run system)
               (error "Component lifecycle tests failed."))))
