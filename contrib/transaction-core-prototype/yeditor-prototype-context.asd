;;;; SPDX-License-Identifier: MIT
(asdf:defsystem "yeditor-prototype-context"
  :description "L2 effect entry point for Yeditor"
  :license "MIT"
  :depends-on ("yeditor-prototype-tx")
  :components ((:file "src/context"))
  :in-order-to ((asdf:test-op (asdf:test-op "yeditor-prototype-tx/tests"))))
