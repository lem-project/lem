;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-effects-tests"
  :license "FSL-1.1-MIT"
  :description "Tests for yeditor-effects"
  :depends-on ("yeditor-effects" "yeditor-tx" "yeditor-context" "rove" "lem-fake-interface")
  :components ((:file "effects-tests") (:file "effects-t4-tests"))
  :perform (asdf:test-op (op c)
             (unless (uiop:symbol-call :rove '#:run :yeditor-effects-tests)
               (error "T4 effects tests failed."))))
