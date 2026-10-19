;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-tx
  (:use :cl)
  (:export #:make-transaction-log #:log-records #:with-transaction
           #:call-with-transaction #:record-effect
           #:install-effect #:commit #:rollback #:undo-effect #:effect-state
           #:transaction-state #:transaction-kind
           #:call-with-workspace-transaction #:with-workspace-transaction
           #:rollback-failure #:rollback-errors #:rollback-cause))
(in-package :yeditor-tx)

(defvar *transaction-log* nil
  "Dynamically scoped destination for effects in the current transaction.")

(defstruct (transaction-log (:constructor make-transaction-log ()))
  "An in-memory effect log owned by one editor execution context."
  (records nil :type list)
  (effects nil :type list)
  (state :open)
  (kind 1))

(defstruct (effect-handle (:constructor make-effect-handle (log owner kind apply undo)))
  "A pending installer and its pre-captured compensation function."
  log owner kind apply undo
  (state :pending))

(define-condition rollback-failure (error)
  ((errors :initarg :errors :reader rollback-errors)
   (cause :initarg :cause :initform nil :reader rollback-cause))
  (:documentation "One or more inverses failed; failed handles can be retried.")
  (:report (lambda (condition stream)
             (format stream "~D inverse(s) failed during rollback.~@[ Original error: ~A~]"
                     (length (rollback-errors condition)) (rollback-cause condition)))))

(defun log-records (log)
  "Return effect alists in recording order, copying the log and record spines.
Payloads and inverse functions retain their identity. LOG is not thread-safe."
  (mapcar (lambda (record) (mapcar #'copy-list record))
          (reverse (transaction-log-records log))))

(defun call-with-transaction (log function)
  "Call FUNCTION with effects directed to LOG, preserving its return values.
Record-effect writes audit entries immediately; install-effect queues work for COMMIT.
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


(defun transaction-state (log)
  "Return LOG's lifecycle: open, committing, committed, rolling-back or rollback result."
  (transaction-log-state log))

(defun transaction-kind (log)
  "Return 1 for an ordinary transaction, or 2 for a workspace side-transaction."
  (transaction-log-kind log))

(defun effect-state (handle)
  "Return HANDLE's pending, active, cancelled, undone or undo-failed state."
  (effect-handle-state handle))

(defun install-effect (comp kind function &key undo-thunk)
  "Queue FUNCTION owned by COMP and return an opaque reversible handle.
UNDO-THUNK is required and captured before FUNCTION runs. It must safely undo
partial installation if FUNCTION signals or exits nonlocally. No work runs until
COMMIT. Installers and inverses must not change this transaction's membership."
  (unless (and *transaction-log*
               (eq :open (transaction-state *transaction-log*)))
    (error "Installing an effect requires an open transaction scope."))
  (check-type function function)
  (check-type undo-thunk function)
  (let ((handle (make-effect-handle *transaction-log* comp kind function undo-thunk)))
    (push handle (transaction-log-effects *transaction-log*))
    handle))

(defun undo-one (handle)
  (case (effect-state handle)
    (:pending (setf (effect-handle-state handle) :cancelled))
    ((:active :undo-failed)
     ;; Retain a retryable handle even if the inverse unwinds nonlocally.
     (setf (effect-handle-state handle) :undo-failed)
     (funcall (effect-handle-undo handle))
     (setf (effect-handle-state handle) :undone)))
  handle)

(defun undo-effect (handle)
  "Cancel a pending HANDLE or undo an applied HANDLE, once.
Independent effects may be uninstalled individually; dependent effects require
LIFO rollback of their whole group. An inverse failure leaves HANDLE retryable."
  (unless (member (transaction-state (effect-handle-log handle))
                  '(:open :committed :rollback-failed))
    (error "Cannot uninstall an effect during commit or rollback."))
  (let* ((log (effect-handle-log handle))
         (state (transaction-state log)))
    (setf (transaction-log-state log) :uninstalling)
    (unwind-protect (undo-one handle)
      (setf (transaction-log-state log) state))))

(defun rollback-log (log &key cause)
  "Cancel pending effects and compensate applied effects in strict LIFO order.
Attempt remaining inverses even if one signals an error, then signal
ROLLBACK-FAILURE. Retry only failed inverses on a subsequent rollback. CAUSE
preserves an original installer error when compensation also fails."
  (when (member (transaction-state log) '(:rolling-back))
    (error "Recursive rollback is not allowed."))
  (when (eq :rolled-back (transaction-state log))
    (return-from rollback-log log))
  (setf (transaction-log-state log) :rolling-back)
  (let ((errors nil)
        (complete-p nil))
    (unwind-protect
         (progn
           (dolist (handle (transaction-log-effects log))
             (handler-case (undo-one handle)
               (error (condition) (push condition errors))))
           (setf complete-p t))
      (setf (transaction-log-state log)
            (if (and complete-p (null errors)) :rolled-back :rollback-failed)))
    (when errors
      (error 'rollback-failure :errors (nreverse errors) :cause cause)))
  log)

(defun rollback (log &key cause)
  "Roll back LOG in LIFO order, retaining failed inverses for explicit retry.
Signal ROLLBACK-FAILURE after attempting all inverses if any fail."
  (unless (member (transaction-state log)
                  '(:open :committed :rolled-back :rollback-failed))
    (error "Cannot start rollback during commit, uninstall or rollback."))
  (rollback-log log :cause cause))

(defun commit (log)
  "Apply pending effects in registration order and record their inverses.
Return LOG; repeated successful commits are idempotent. On failure or nonlocal
exit, compensate all attempted installers, including the failing installer.
An already rolled-back transaction cannot be reused."
  (when (eq :committed (transaction-state log))
    (return-from commit log))
  (unless (eq :open (transaction-state log))
    (error "Commit requires an open transaction."))
  (setf (transaction-log-state log) :committing)
  (let ((complete-p nil)
        (cause nil)
        (*transaction-log* log))
    (unwind-protect
         (handler-case
             (progn
               (dolist (handle (reverse (transaction-log-effects log)))
                 (when (eq :pending (effect-state handle))
                   (setf (effect-handle-state handle) :active)
                   (let ((payload (funcall (effect-handle-apply handle))))
                     (record-effect (effect-handle-kind handle)
                                    (effect-handle-owner handle) payload
                                    (effect-handle-undo handle)))))
               (setf (transaction-log-state log) :committed
                     complete-p t))
           (error (condition) (setf cause condition)))
      (unless complete-p
        (rollback-log log :cause cause)))
    ;; Signal only after cleanup, so an unhandled installer error cannot enter
    ;; the debugger before compensation has run.
    (when cause (error cause)))
  log)

(defun call-with-workspace-transaction (owner function)
  "Collect a kind-2 child transaction and queue it as one effect of the parent.
FUNCTION registers reversible buffer modifications in the child. Commit and
rollback execute the whole group without calling or clearing buffer undo.
The caller supplies edits/inverses appropriate to the buffer's undo protocol."
  (unless (and *transaction-log* (eq :open (transaction-state *transaction-log*)))
    (error "Workspace side-transactions require an open parent transaction."))
  (let ((child (make-transaction-log)))
    (setf (transaction-log-kind child) 2)
    (with-transaction (child) (funcall function))
    (install-effect owner :workspace
                    (lambda () (commit child))
                    :undo-thunk (lambda () (rollback child)))))

(defmacro with-workspace-transaction ((owner) &body body)
  "Queue BODY's effects as one reversible workspace group owned by OWNER."
  `(call-with-workspace-transaction ,owner (lambda () ,@body)))
