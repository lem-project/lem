;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-effects-t6-tests"
  :license "FSL-1.1-MIT"
  :description "T6 tests: process, fdefinition effects (integration)"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-effects" "rove")
  :components ((:file "effects-t6-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call :rove '#:run c)))
