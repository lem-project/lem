;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-tx/tests
  (:use :cl)
  (:export #:run-tests))
(in-package :yeditor-tx/tests)

(defun run-tests ()
  "Exercise log retention, ordering, isolation, and transaction scope."
  (let ((log (yeditor-tx:make-transaction-log))
        (inverse (lambda () nil)))
    (assert (null (yeditor-tx:log-records log)))
    (assert (= 42 (yeditor-tx:with-transaction (log) 42)))
    (yeditor-tx:with-transaction (log)
      (dotimes (i 100)
        (yeditor-tx:record-effect :noop :test i inverse)))
    (let ((records (yeditor-tx:log-records log)))
      (assert (= 100 (length records)))
      (loop :for record :in records :for i :from 0
            :do (assert (eq :noop (cdr (assoc :kind record))))
                (assert (eq :test (cdr (assoc :owner record))))
                (assert (= i (cdr (assoc :payload record))))
                (assert (eq inverse (cdr (assoc :inverse record))))
                (assert (integerp (cdr (assoc :ts record))))))
    (let ((other (yeditor-tx:make-transaction-log)))
      (yeditor-tx:with-transaction (log)
        (yeditor-tx:with-transaction (other)
          (yeditor-tx:record-effect :noop :nested nil inverse))
        (yeditor-tx:record-effect :noop :outer nil inverse))
      (assert (= 101 (length (yeditor-tx:log-records log))))
      (assert (= 1 (length (yeditor-tx:log-records other)))))
    (handler-case
        (yeditor-tx:with-transaction (log)
          (yeditor-tx:record-effect :noop :error nil inverse)
          (error "Expected failure"))
      (error () nil))
    ;; T1 records immediately; rollback belongs to T2.
    (assert (= 102 (length (yeditor-tx:log-records log)))))
  (dotimes (cycle 100)
    (let ((log (yeditor-tx:make-transaction-log)))
      (assert (equal '(1 2)
                     (multiple-value-list
                      (yeditor-tx:with-transaction (log) (values 1 2)))))
      (yeditor-tx:with-transaction (log)
        (yeditor-tx:record-effect :noop :cycle cycle (lambda () nil)))
      (let ((snapshot (yeditor-tx:log-records log)))
        (setf (cdr (assoc :kind (first snapshot))) :changed)
        (assert (eq :noop (cdr (assoc :kind
                                         (first (yeditor-tx:log-records log)))))))))
  (assert (handler-case
              (progn (yeditor-tx:record-effect :noop :test nil (lambda () nil)) nil)
            (error () t)))
  (format t "Transaction tests passed (100-effect retention, order, isolation, scope, error path).~%")
  t)
