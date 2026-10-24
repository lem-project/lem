;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-bench-tests
  (:use :cl :rove))
(in-package :yeditor-bench-tests)

(defun results (&optional (p95 100))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name '("command-dispatch" "keystroke-to-frame" "theme-install-uninstall" "tx-commit-empty"))
      (let ((row (make-hash-table :test #'equal))
            (value (if (equal name "tx-commit-empty") 1 p95)))
        (setf (gethash "p50" row) value (gethash "p95" row) value
              (gethash "p99" row) value (gethash "iterations" row) 100
              (gethash name table) row)))
    table))

(deftest regression-boundaries-and-budgets
  (ok (nth-value 1 (yeditor-bench:compare-results (results 120) (results 100))))
  (multiple-value-bind (report pass) (yeditor-bench:compare-results (results 121) (results 100))
    (ng pass)
    (ok (search "FAIL" report)))
  (ng (nth-value 1 (yeditor-bench:compare-results (results 33001))))
  (ok (nth-value 1 (yeditor-bench:compare-results (results 100)))))

(deftest zero-baseline-and-clock-noise
  (dolist (value '(0 1 2 3))
    (let ((base (results)) (head (results)))
      (setf (gethash "p50" (gethash "tx-commit-empty" base)) 0
            (gethash "p95" (gethash "tx-commit-empty" base)) 0
            (gethash "p99" (gethash "tx-commit-empty" base)) 0)
      (dolist (key '("p50" "p95" "p99"))
        (setf (gethash key (gethash "tx-commit-empty" head)) value))
      (multiple-value-bind (report pass) (yeditor-bench:compare-results head base)
        (ok (stringp report))
        (ok (eql pass (<= value 2)) "zero baseline produces a decision without division by zero")))))

(deftest incomplete-results-fail-closed
  (ng (nth-value 1 (yeditor-bench:compare-results (make-hash-table :test #'equal))))
  (let ((base (results)))
    (remhash "theme-install-uninstall" base)
    (ng (nth-value 1 (yeditor-bench:compare-results (results) base))))
  (let ((base (results)))
    (setf (gethash "iterations" (gethash "command-dispatch" base)) 101)
    (ng (nth-value 1 (yeditor-bench:compare-results (results) base))))
  (dolist (bad '(-1 "invalid" nil))
    (let ((head (results)))
      (setf (gethash "p95" (gethash "command-dispatch" head)) bad)
      (ng (nth-value 1 (yeditor-bench:compare-results head)))))
  (let ((head (results)))
    (setf (gethash "iterations" (gethash "command-dispatch" head)) 0)
    (ng (nth-value 1 (yeditor-bench:compare-results head)))))

(deftest real-theme-cycle-100-times
  (lem:save-excursion
    (lem/buffer/buffer-list-manager:with-current-buffers ()
      (lem-fake-interface:with-fake-interface ()
        (setf (lem:current-buffer) (lem:window-buffer (lem:current-window)))
        (lem:load-theme "lem-default" nil)
        (let* ((component 'yeditor-components:le-themes-ergonomic)
               (entry (gethash component yeditor-context:*component-registry*))
               (prior (getf entry :activate))
               (palette yeditor-components/palette:*palette-themes-function*)
               (activations 0)
               (faces (loop :for name :in '(lem:syntax-keyword-attribute lem:syntax-comment-attribute lem:region)
                            :for a := (lem:ensure-attribute name nil)
                            :collect (list (lem:attribute-foreground a) (lem:attribute-background a)))))
          (unwind-protect
               (progn
                 (setf (getf entry :activate)
                       (lambda (c)
                         (declare (ignore c))
                         (incf activations)
                         (ok (equal (lem:current-theme) "ergo-gruvbox-soft"))))
                 (dotimes (i 100)
                   (yeditor-bench::theme-cycle)
                   (ok (equal (lem:current-theme) "lem-default"))
                   (ok (eq palette yeditor-components/palette:*palette-themes-function*))
                   (ok (eq :inactive (getf entry :status)))
                   (ok (equal faces
                              (loop :for name :in '(lem:syntax-keyword-attribute lem:syntax-comment-attribute lem:region)
                                    :for a := (lem:ensure-attribute name nil)
                                    :collect (list (lem:attribute-foreground a) (lem:attribute-background a)))))))
            (setf (getf entry :activate) prior)
            (yeditor-context:uninstall-component component))
          (ok (= activations 100)))))))

(deftest theme-failure-and-borrowed-state
  (lem:save-excursion
    (lem/buffer/buffer-list-manager:with-current-buffers ()
      (lem-fake-interface:with-fake-interface ()
        (setf (lem:current-buffer) (lem:window-buffer (lem:current-window)))
        (lem:load-theme "lem-default" nil)
        (let* ((component 'yeditor-components:le-themes-ergonomic)
               (entry (gethash component yeditor-context:*component-registry*))
               (prior (getf entry :activate))
               (palette yeditor-components/palette:*palette-themes-function*))
          (unwind-protect
               (progn
                 (setf (getf entry :activate) (lambda (c) (declare (ignore c)) (error "activation failed")))
                 (ok (signals (yeditor-bench::theme-cycle) 'error))
                 (ok (equal (lem:current-theme) "lem-default"))
                 (ok (eq palette yeditor-components/palette:*palette-themes-function*))
                 (ok (eq :inactive (getf entry :status)))
                 (setf (getf entry :activate) prior)
                 (yeditor-context:install-component component)
                 (ok (signals (yeditor-bench::theme-cycle) 'error))
                 (ok (eq :active (getf entry :status)) "benchmark preserves a session-owned component"))
            (setf (getf entry :activate) prior)
            (yeditor-context:uninstall-component component)))))))
