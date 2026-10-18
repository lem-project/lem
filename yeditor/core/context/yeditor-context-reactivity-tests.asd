;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-context-reactivity-tests"
  :license "FSL-1.1-MIT"
  :description "T7 tests: coeffect classification, context-event dispatch, work-cycle predicates"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-reactivity" "rove")
  :components ((:file "reactivity-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
