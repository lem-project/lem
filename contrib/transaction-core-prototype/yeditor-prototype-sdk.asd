;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-prototype-sdk"
  :description "Reversible component lifecycle for Yeditor"
  :license "MIT"
  :depends-on ("yeditor-prototype-context")
  :components ((:file "src/sdk"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-prototype-sdk/tests"))))

(asdf:defsystem "yeditor-prototype-sdk/tests"
  :depends-on ("yeditor-prototype-sdk")
  :components ((:file "tests/sdk-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :yeditor-prototype-sdk/tests :run-tests)))

(asdf:defsystem "yeditor-prototype-sdk/rove-tests"
  :depends-on ("yeditor-prototype-sdk/tests" "rove")
  :components ((:file "tests/sdk-rove-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op))
             (unless (uiop:symbol-call :rove :run system)
               (error "Component lifecycle tests failed."))))
