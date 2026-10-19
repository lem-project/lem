;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-context"
  :description "L2 effect entry point for Yeditor"
  :license "MIT"
  :depends-on ("yeditor-tx")
  :components ((:file "yeditor/context"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-tx/tests"))))
