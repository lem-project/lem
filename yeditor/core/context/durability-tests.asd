;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "durability-tests"
  :license "FSL-1.1-MIT"
  :description "T9 tests: effects log, replay, crash recovery, replay benchmark"
  :depends-on ("yeditor-tx" "yeditor-context" "yeditor-context-durability" "rove")
  :components ((:file "durability-tests")))
