;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; T10 benchmarks. Usage (repository root):
;;;   sbcl --script scripts/run-yeditor-bench.lisp OUT.json [BASE.json]
;;; Writes OUT.json and prints the report; with BASE.json, exits 1 on a regression
;;; (p95 more than 20% slower) or on a p95 over its absolute budget.
(load ".qlot/setup.lisp")
(asdf:load-system :yeditor-bench)
(let* ((args (uiop:command-line-arguments))
       (out (or (first args) "bench-results.json"))
       (base-path (second args)))
  (yeditor-bench:write-results (yeditor-bench:run-benchmarks) out)
  (multiple-value-bind (report ok)
      (yeditor-bench:compare-results (yeditor-bench:read-results out)
                                     (and base-path (probe-file base-path)
                                          (yeditor-bench:read-results base-path)))
    (write-string report)
    (uiop:quit (if ok 0 1))))
