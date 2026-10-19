;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-bench
  (:use :cl :lem)
  (:export :run-benchmarks
           :write-results
           :read-results
           :compare-results
           :*budgets-us*))
(in-package :yeditor-bench)

(defparameter *rounds* 3
  "Each benchmark runs this many rounds; the reported percentiles are the median across rounds.")

(defparameter *budgets-us*
  (list (cons "keystroke-to-frame" 33000)
        (cons "command-dispatch" 150000)
        (cons "tx-commit-empty" 10))
  "Absolute p95 budgets in microseconds (plan Cap. 3.2 P4 and T1).")

;; get-internal-real-time is millisecond-resolution on some SBCL builds; use the
;; microsecond time-of-day clock for timings.
(defun now-us ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000000) usec)))

(defun percentile (sorted p)
  (let ((n (length sorted)))
    (aref sorted (min (1- n) (floor (* p n))))))

(defun measure (thunk iterations &key (warmup 200))
  "Run THUNK WARMUP times, then ITERATIONS timed times; return (p50 p95 p99) in microseconds."
  (dotimes (i warmup) (funcall thunk))
  (let ((samples (make-array iterations)))
    (dotimes (i iterations)
      (let ((t0 (now-us)))
        (funcall thunk)
        (setf (aref samples i) (- (now-us) t0))))
    (let ((sorted (sort samples (function <))))
      (mapcar (lambda (p) (float (percentile sorted p))) (list 0.5 0.95 0.99)))))

(defun median-of-rounds (thunk iterations)
  (let ((rounds (loop :repeat *rounds* :collect (measure thunk iterations))))
    (loop :for k :from 0 :below 3
          :collect (let ((v (sort (mapcar (lambda (r) (nth k r)) rounds) (function <))))
                     (nth (floor (length v) 2) v)))))

(defun large-buffer (lines)
  (let ((buffer (make-buffer "*yeditor-bench*" :enable-undo-p nil)))
    (with-point ((p (buffer-start-point buffer) :left-inserting))
      (dotimes (i lines)
        (insert-string p (format nil "(defun function-~D (x) (+ x ~D)) ; line ~D~%" i i i))))
    buffer))

(defun benchmarks ()
  "List of (name iterations thunk) run inside a fake-interface session."
  (let ((buffer (large-buffer 10000)))
    (switch-to-buffer buffer)
    (move-to-line (current-point) 5000)
    (list
     (list "tx-commit-empty" 20000
           (lambda () (yeditor-tx:with-transaction (yeditor-tx:tx-commit))))
     (list "command-dispatch" 5000
           (lambda () (call-command (quote next-line) nil) (call-command (quote previous-line) nil)))
     (list "keystroke-to-frame" 2000
           (lambda ()
             (insert-character (current-point) #\x)
             (redraw-display)
             (delete-previous-char)))
     (list "component-toggle" 2000
           (lambda ()
             (yeditor-context:install-component (quote yeditor-components:le-zero-chrome))
             (yeditor-context:uninstall-component (quote yeditor-components:le-zero-chrome)))))))

(defun run-benchmarks ()
  "Run every benchmark; return an alist (name . plist :p50 :p95 :p99 :iterations), microseconds."
  (lem-fake-interface:with-fake-interface ()
    (lem/common/timer:with-timer-manager (make-instance (quote lem/common/timer:timer-manager))
      (loop :for (name iterations thunk) :in (benchmarks)
            :collect (destructuring-bind (p50 p95 p99) (median-of-rounds thunk iterations)
                       (cons name (list :p50 p50 :p95 p95 :p99 p99 :iterations iterations)))))))

(defun write-results (results path)
  (let ((table (make-hash-table :test (function equal))))
    (loop :for (name . plist) :in results
          :do (let ((h (make-hash-table :test (function equal))))
                (loop :for (k v) :on plist :by (function cddr)
                      :do (setf (gethash (string-downcase (symbol-name k)) h) v))
                (setf (gethash name table) h)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (yason:encode table out))))

(defun read-results (path)
  (with-open-file (in path) (yason:parse in)))

(defparameter *regression-ratio* 1.2
  "Fail when head p95 exceeds base p95 by more than 20%.")

(defparameter *noise-floor-us* 2
  "Ignore regressions smaller than this many microseconds in absolute terms (timer noise).")

(defun compare-results (head &optional base)
  "HEAD and BASE are hash tables from read-results. Return (values report-string ok-p).
   Fails on a p95 over budget, or a p95 regression over 20% and over the noise floor."
  (let ((ok t))
    (values
     (with-output-to-string (s)
       (format s "~&~22A ~10A ~10A ~10A ~12A ~A~%" "benchmark" "p50 us" "p95 us" "p99 us" "base p95" "status")
       (loop :for name :in (sort (alexandria:hash-table-keys head) (function string<))
             :do (let* ((h (gethash name head))
                        (p95 (gethash "p95" h))
                        (b (and base (gethash name base)))
                        (bp95 (and b (gethash "p95" b)))
                        (budget (cdr (assoc name *budgets-us* :test (function string=))))
                        (status (cond ((and budget (> p95 budget))
                                       (setf ok nil) (format nil "FAIL over budget (~D us)" budget))
                                      ((and bp95 (> p95 (* *regression-ratio* bp95))
                                            (> (- p95 bp95) *noise-floor-us*))
                                       (setf ok nil) (format nil "FAIL +~D% vs base"
                                                             (round (* 100 (- (/ p95 bp95) 1)))))
                                      ((and bp95 (< (max p95 bp95) *noise-floor-us*)) "ok (below noise floor)")
                                      (bp95 (format nil "ok (~@D%)" (round (* 100 (- (/ p95 bp95) 1)))))
                                      (t "ok (no base)"))))
                   (format s "~22A ~10,1F ~10,1F ~10,1F ~12A ~A~%"
                           name (gethash "p50" h) p95 (gethash "p99" h)
                           (if bp95 (format nil "~,1F" bp95) "-") status)))
       (format s "~&Keystroke-to-frame runs the full redisplay pipeline against the fake frontend~%(no terminal I/O); it measures editor-side cost.~%"))
     ok)))
