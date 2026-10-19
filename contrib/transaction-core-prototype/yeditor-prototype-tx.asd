;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-prototype-tx"
  :description "In-memory transaction log for Yeditor"
  :license "MIT"
  :serial t
  :components ((:file "src/tx"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-prototype-tx/tests"))))

(asdf:defsystem "yeditor-prototype-tx/tests"
  :depends-on ("yeditor-prototype-tx" "yeditor-prototype-context")
  :components ((:file "tests/tx-tests")
               (:file "tests/commit-tests")
               (:file "tests/suspension-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :yeditor-prototype-tx/tests :run-tests)
             (uiop:symbol-call :yeditor-prototype-tx/commit-tests :run-tests)
             (uiop:symbol-call :yeditor-prototype-tx/suspension-tests :run-tests)))

(asdf:defsystem "yeditor-prototype-tx/lem-tests"
  :description "Workspace transaction tests against real Lem buffers"
  :depends-on ("yeditor-prototype-tx/tests" "lem/core" "rove")
  :components ((:file "tests/workspace-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op))
             (unless (uiop:symbol-call :rove :run system)
               (error "Workspace integration tests failed."))))
