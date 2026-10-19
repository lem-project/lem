;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-tx
  (:use :cl)
  (:export #:make-transaction-log #:log-records #:with-transaction
           #:call-with-transaction #:record-effect))
(in-package :yeditor-tx)

(defvar *transaction-log* nil
  "Dynamically scoped destination for effects in the current transaction.")

(defstruct (transaction-log (:constructor make-transaction-log ()))
  "An in-memory effect log owned by one editor execution context."
  (records nil :type list))

(defun log-records (log)
  "Return effect alists in recording order, copying the log and record spines.
Payloads and inverse functions retain their identity. LOG is not thread-safe."
  (mapcar (lambda (record) (mapcar #'copy-list record))
          (reverse (transaction-log-records log))))

(defun call-with-transaction (log function)
  "Call FUNCTION with effects directed to LOG, preserving its return values.
T1 records effects immediately; commit grouping and rollback are not implemented.
Nested scopes restore the enclosing log even when FUNCTION signals an error."
  (check-type log transaction-log)
  (let ((*transaction-log* log))
    (funcall function)))

(defmacro with-transaction ((log) &body body)
  "Evaluate BODY with effects recorded in LOG. See CALL-WITH-TRANSACTION."
  `(call-with-transaction ,log (lambda () ,@body)))

(defun record-effect (kind owner payload inverse)
  "Record KIND, OWNER, PAYLOAD, INVERSE and a monotonic timestamp.
Require an active transaction and a function INVERSE. Do not execute the effect
or its inverse; callers capture the inverse when applying the effect."
  (unless *transaction-log*
    (error "Recording an effect requires WITH-TRANSACTION."))
  (check-type inverse function)
  (let ((record (list (cons :kind kind) (cons :owner owner)
                      (cons :payload payload) (cons :inverse inverse)
                      (cons :ts (get-internal-real-time)))))
    (push record (transaction-log-records *transaction-log*))
    record))
