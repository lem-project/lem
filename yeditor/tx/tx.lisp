(defpackage :yeditor-tx
  (:use :cl)
  (:export #:with-transaction
           #:install-effect
           #:*tx-log*))
(in-package :yeditor-tx)

(defvar *tx-log* nil
  "In-memory transaction log – list of alists with keys :kind :owner :payload :inverse :ts.")

(defun install-effect (kind owner payload inverse)
  "Record a transaction effect in the in-memory log.
   KIND, OWNER, PAYLOAD, INVERSE are stored as provided.
   TS is the current universal time.
   Returns the new log entry."
  (let ((entry (list (cons :kind kind)
                     (cons :owner owner)
                     (cons :payload payload)
                     (cons :inverse inverse)
                     (cons :ts (get-universal-time)))))
    (push entry *tx-log*)
    entry))

(defmacro with-transaction (&body body)
  "Execute BODY, recording any effects via `install-effect`.
   Currently a no-op wrapper – just evaluate BODY.
   Returns result of last form."
  `(progn ,@body))
