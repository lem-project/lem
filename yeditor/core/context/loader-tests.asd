;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "loader-tests"
  :license "FSL-1.1-MIT"
  :description "T8 tests: config loader, reconciliation, delta application"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-loader" "rove" "lem-fake-interface")
  :components ((:file "loader-tests")
               (:file "loader-lifecycle-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :rove '#:run c) (error "Loader tests failed."))))
