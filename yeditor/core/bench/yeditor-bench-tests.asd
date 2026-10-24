;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-bench-tests"
  :license "FSL-1.1-MIT"
  :depends-on ("yeditor-bench" "rove")
  :components ((:file "bench-tests"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :rove '#:run c) (error "Benchmark tests failed."))))
