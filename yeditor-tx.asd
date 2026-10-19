;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-tx"
  :description "In-memory transaction log for Yeditor"
  :license "MIT"
  :serial t
  :components ((:file "yeditor/tx/tx"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-tx/tests"))))

(asdf:defsystem "yeditor-tx/tests"
  :depends-on ("yeditor-tx" "yeditor-context")
  :components ((:file "yeditor/tests/tx-tests")
               (:file "yeditor/tests/commit-tests")
               (:file "yeditor/tests/suspension-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :yeditor-tx/tests :run-tests)
             (uiop:symbol-call :yeditor-tx/commit-tests :run-tests)
             (uiop:symbol-call :yeditor-tx/suspension-tests :run-tests)))

(asdf:defsystem "yeditor-tx/lem-tests"
  :description "Workspace transaction tests against real Lem buffers"
  :depends-on ("yeditor-tx/tests" "lem/core" "rove")
  :components ((:file "yeditor/tests/workspace-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op))
             (unless (uiop:symbol-call :rove :run system)
               (error "Workspace integration tests failed."))))
