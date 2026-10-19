;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-bench"
  :description "T10 perceived-speed benchmarks: p50/p95/p99, budgets, regression comparison"
  :license "FSL-1.1-MIT"
  :depends-on ("lem/core" "lem-fake-interface" "yason" "yeditor-core" "yeditor-components")
  :components ((:file "bench")))
