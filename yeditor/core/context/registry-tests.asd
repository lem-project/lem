;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "registry-tests"
  :license "FSL-1.1-MIT"
  :depends-on ("yeditor-context" "yeditor-tx" "rove")
  :components ((:file "registry-test"))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :rove '#:run c)))
