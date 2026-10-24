;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; T10 benchmarks. Usage (repository root):
;;;   sbcl --script scripts/run-yeditor-bench.lisp OUT.json [BASE.json]
;;; Baseline collection: sbcl --script scripts/run-yeditor-bench.lisp --measure-only OUT.json
;;; Writes OUT.json and prints the report; with BASE.json, exits 1 on a regression
;;; (p95 more than 50% slower) or on a p95 over its absolute budget.
(load ".qlot/setup.lisp")
(asdf:load-system :yeditor-bench)
(let* ((args (uiop:command-line-arguments))
       (measure-only (equal (first args) "--measure-only"))
       (paths (if measure-only (rest args) args))
       (out (or (first paths) "bench-results.json"))
       (base-path (second paths)))
  (when (or (> (length paths) (if measure-only 1 2))
            (and measure-only (null paths)))
    (error "Usage: run-yeditor-bench.lisp [--measure-only] OUT.json [BASE.json]"))
  (when (and base-path (not (probe-file base-path)))
    (error "Requested benchmark baseline does not exist: ~A" base-path))
  (yeditor-bench:write-results (yeditor-bench:run-benchmarks) out)
  (if measure-only
      (progn (format t "Measured baseline: ~A~%" out) (uiop:quit 0))
      (multiple-value-bind (report ok)
          (yeditor-bench:compare-results (yeditor-bench:read-results out)
                                         (and base-path (yeditor-bench:read-results base-path)))
        (write-string report)
        (uiop:quit (if ok 0 1)))))
