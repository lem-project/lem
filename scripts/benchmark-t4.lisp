;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/benchmark-t4 (:use :cl))
(in-package :yeditor-effects/benchmark-t4)

(load ".qlot/setup.lisp")
(require :asdf)
(dolist (path '("yeditor/core/tx/yeditor-tx.asd"
                "yeditor/core/context/yeditor-context.asd"
                "yeditor/effects/yeditor-effects.asd"))
  (asdf:load-asd (merge-pathnames path (uiop:getcwd))))
(ql:quickload "yeditor-effects" :silent t)

(defun now-us ()
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000) microseconds)))

(defun install-sample ()
  (let ((yeditor-tx:*tx-pending* nil) (yeditor-tx:*tx-committed* nil))
    (unwind-protect
         (let ((start (now-us)))
           (yeditor-effects:install-buffer :benchmark "*t4-benchmark*")
           (yeditor-tx:tx-commit)
           (- (now-us) start))
      ;; Cleanup is outside the installation timing, but required every sample.
      (yeditor-tx:tx-rollback)
      (when (lem:get-buffer "*t4-benchmark*")
        (error "Benchmark buffer leaked.")))))

(lem:with-current-buffers ()
  (dotimes (i 200) (install-sample))
  (let ((samples (make-array 10000)))
    (dotimes (i (length samples))
      (setf (aref samples i) (install-sample)))
    (when (find-if #'minusp samples)
      (error "Wall clock moved backwards; benchmark is invalid."))
    (sort samples #'<)
    (let ((p50 (aref samples 4999)) (p95 (aref samples 9499))
          (p99 (aref samples 9899)))
      (format t "T4 e-buffer: 10000 fresh installations, p50=~D us p95=~D us p99=~D us.~%"
              p50 p95 p99)
      (format t "Measures queue + commit + real buffer creation; cleanup excluded; no UI/redisplay.~%")
      (unless (null (lem:buffer-list)) (error "Benchmark changed the buffer registry."))
      (when (>= p95 50) (error "T4 p95 budget exceeded: ~D us (target < 50 us)." p95)))))
