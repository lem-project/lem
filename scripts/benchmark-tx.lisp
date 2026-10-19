;;;; SPDX-License-Identifier: MIT
(require :asdf)
(asdf:load-asd (merge-pathnames "../yeditor-tx.asd" *load-truename*))
(asdf:load-system "yeditor-tx")

;; Batch empty scopes to exceed the timer resolution. Each sample reports
;; amortized microseconds per scope, not individual transaction latency.
(let ((log (yeditor-tx:make-transaction-log))
      (batch-size 1000)
      (sample-count 1000))
  (dotimes (warmup 10000)
    (yeditor-tx:with-transaction (log) nil))
  (dotimes (run 3)
    (let ((samples
            (sort
             (loop :repeat sample-count
                   :collect
                   (let ((start (get-internal-run-time)))
                     (dotimes (i batch-size)
                       (yeditor-tx:with-transaction (log) nil))
                     (/ (* 1000000d0 (- (get-internal-run-time) start))
                        (* internal-time-units-per-second batch-size))))
             #'<)))
      (format t "Run ~D: empty scope amortized CPU time p50=~,3F us p95=~,3F us (~D samples x ~D scopes)~%"
              (1+ run) (nth 499 samples) (nth 949 samples)
              sample-count batch-size))))
