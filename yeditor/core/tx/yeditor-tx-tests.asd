;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-tx-tests"
  :license "FSL-1.1-MIT"
  :description "Tests for yeditor-tx"
  :depends-on ("yeditor-tx" "rove")
  :components ((:file "tx-test") (:file "tx2-test") (:file "side-test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-tx-tests)))
