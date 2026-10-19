;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-effects-tests"
  :license "FSL-1.1-MIT"
  :description "Tests for yeditor-effects"
  :depends-on ("yeditor-effects" "yeditor-tx" "yeditor-context" "rove")
  :components ((:file "effects-tests"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove '#:run :yeditor-effects-tests)))
