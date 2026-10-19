;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-tx"
  :description "In-memory transaction log for Yeditor"
  :license "MIT"
  :serial t
  :components ((:file "yeditor/tx/tx"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-tx/tests"))))

(asdf:defsystem "yeditor-tx/tests"
  :depends-on ("yeditor-tx")
  :components ((:file "yeditor/tests/tx-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :yeditor-tx/tests :run-tests)))
