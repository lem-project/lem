;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.
(defsystem "yeditor-navigation-tests"
  :license "FSL-1.1-MIT"
  :depends-on ("yeditor-components" "lem-fake-interface" "rove")
  :components ((:file "navigation-tests"))
  :perform (test-op (op c) (unless (uiop:symbol-call :rove '#:run c) (error "Navigation tests failed."))))
