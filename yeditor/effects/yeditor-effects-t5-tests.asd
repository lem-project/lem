;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-effects-t5-tests"
  :license "FSL-1.1-MIT"
  :description "T5 tests: keymap, variable, command, overlay, timer effects"
  :depends-on ("lem/core" "yeditor-tx" "yeditor-context" "yeditor-effects" "rove")
  :components ((:file "effects-t5-tests")
               (:file "effects-t5-lifecycle-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :rove '#:run c)
               (error "T5 effect tests failed."))))
