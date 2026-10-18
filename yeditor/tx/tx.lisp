(defpackage :yeditor-tx
  (:use :cl)
  (:export #:with-transaction
           #:install-effect
           #:tx-commit
           #:tx-rollback
           #:*tx-pending*
           #:*tx-committed*))
(in-package :yeditor-tx)

(defvar *tx-pending* nil
  "Pending transaction effects – list of alists with keys :comp :kind :fn :inverse :ts.")
(defvar *tx-committed* nil
  "Committed transaction effects for possible rollback.")

(defun install-effect (comp kind fn &key undo-thunk)
  "Record a transaction effect.
   COMP and KIND are stored for identification.
   FN is the effect function to execute on commit.
   UNDO-THUNK is the function to undo the effect, stored as :inverse.
   Returns a handle (the log entry)."
  (let ((entry (list (cons :comp comp)
                     (cons :kind kind)
                     (cons :fn fn)
                     (cons :inverse undo-thunk)
                     (cons :ts (get-internal-real-time)))))
    (push entry *tx-pending*)
    entry))

(defun tx-commit ()
  "Execute pending effects in registration order and store inverses for rollback.
   If any effect signals an error, already executed effects are rolled back and the error is re‑raised."
  (let ((executed nil))
    (handler-case
        (progn
          (dolist (e (reverse *tx-pending*))
            (let ((fn (cdr (assoc :fn e))))
              (funcall fn)
              (push e executed)))
          (setf *tx-committed* (reverse executed))
          (setf *tx-pending* nil))
      (error (e)
        ;; rollback any effects that were already executed
        (dolist (e executed)
          (let ((undo (cdr (assoc :inverse e))))
            (when undo (funcall undo))))
        (setf *tx-pending* nil)
        (setf *tx-committed* nil)
        (error e)))))


(defun tx-rollback ()
  "Rollback committed effects by executing their inverses in LIFO order."
  (when *tx-committed*
    (dolist (e (reverse *tx-committed*))
      (let ((undo (cdr (assoc :inverse e))))
        (when undo (funcall undo))))
    (setf *tx-committed* nil)))

(defmacro with-transaction (&body body)
  "Execute BODY, allowing effects to be installed via `install-effect`.
   Returns result of BODY."
  `(progn ,@body))