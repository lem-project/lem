;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context/durability
  (:use :cl)
  (:import-from :yeditor-context
   #:*component-registry*
   #:*lifecycle-hook*
   #:install-component)
  (:export
   #:*effects-log-path*
   #:default-effects-log-path
   #:log-event
   #:enable-durability
   #:disable-durability
   #:replay-log
   #:recover))
(in-package :yeditor-context/durability)

(defvar *effects-log-path* nil
  "Path of the append-only JSONL effects log; nil selects the XDG data directory.")
(defvar *log-lock* (sb-thread:make-mutex :name "yeditor-effects-log"))
(defvar *durability-hook* nil)
(defvar *previous-hook* nil)

(defun default-effects-log-path ()
  "Return the effects journal in the user's XDG data directory."
  (uiop:xdg-data-home "yeditor/effects.log"))

(defun log-path () (or *effects-log-path* (default-effects-log-path)))

(defun component-id (component)
  (unless (and (symbolp component) (symbol-package component))
    (error "A durable component needs an interned symbol: ~S" component))
  (format nil "~A::~A" (package-name (symbol-package component)) (symbol-name component)))

(defun find-component-symbol (id)
  (let ((sep (search "::" id)))
    (when sep
      (let ((pkg (find-package (subseq id 0 sep))))
        (when pkg (find-symbol (subseq id (+ sep 2)) pkg))))))

(defun journal-event-p (event)
  (member event '("install-begin" "install-commit" "install-abort"
                  "install-cancelled" "uninstall" "suspend" "resume") :test #'equal))

(defun log-event (event component &key (path (log-path)))
  "Append and flush one JSONL event. Preserve torn tails, separating the next record.
The mutex serializes writers in this session; the journal has one owning process."
  (let* ((event-name (string-downcase (symbol-name event)))
         (id (if (symbolp component) (component-id component) component))
         (record (make-hash-table :test #'equal)))
    (unless (and (journal-event-p event-name) (stringp id) (plusp (length id)))
      (error "Invalid lifecycle event ~S for ~S" event component))
    (setf (gethash "ts" record) (get-universal-time)
          (gethash "event" record) event-name
          (gethash "component" record) id)
    ;; Encode before opening the journal: encoding failure cannot leave half a record.
    (let ((bytes (sb-ext:string-to-octets
                  (with-output-to-string (out) (yason:encode record out))
                  :external-format :utf-8)))
      (sb-thread:with-mutex (*log-lock*)
        (ensure-directories-exist path)
        (let ((needs-newline
                (when (probe-file path)
                  (with-open-file (in path :element-type '(unsigned-byte 8))
                    (let ((size (file-length in)))
                      (when (plusp size)
                        (file-position in (1- size))
                        (/= 10 (read-byte in))))))))
          (with-open-file (out path :direction :output :if-exists :append
                                    :if-does-not-exist :create :element-type '(unsigned-byte 8))
            (when needs-newline (write-byte 10 out))
            (write-sequence bytes out)
            (write-byte 10 out)
            (finish-output out)))))))

(defun make-journal-hook (prior path)
  (lambda (event component)
    ;; A rejected transition must not be recorded as committed.
    (when prior (funcall prior event component))
    (log-event event component :path path)))

(defun enable-durability (&key path)
  "Journal lifecycle transitions, preserving the previous observer. Idempotent.
Call after definitions/configuration are loaded, before RECOVER and session changes."
  (when path (setf *effects-log-path* path))
  (unless (eq *lifecycle-hook* *durability-hook*)
    (setf *previous-hook* *lifecycle-hook*))
  (setf *durability-hook* (make-journal-hook *previous-hook* (log-path))
        *lifecycle-hook* *durability-hook*))

(defun disable-durability ()
  "Remove our observer and restore its predecessor, preserving later replacements."
  (when (and *durability-hook* (eq *lifecycle-hook* *durability-hook*))
    (setf *lifecycle-hook* *previous-hook*))
  (setf *durability-hook* nil *previous-hook* nil))

(defun replay-log (&key (path (log-path)))
  "Return LIVE ids in last activation order and CANCELLED pending install ids.
Only recognized, well-formed records affect reconciliation. Invalid records, including
preserved torn writes, are ignored and counted in the third return value for audit.
No Lisp code, component definitions or inverse closures are read from the journal."
  (let ((last (make-hash-table :test #'equal)) (index 0) (ignored 0))
    (when (probe-file path)
      (with-open-file (in path :external-format '(:utf-8 :replacement #\Replacement_Character))
        (loop :for line := (read-line in nil)
              :while line
              :do (incf index)
                  (let* ((rec (ignore-errors (yason:parse line)))
                         (id (and (hash-table-p rec) (gethash "component" rec)))
                         (event (and (hash-table-p rec) (gethash "event" rec))))
                    (if (and (stringp id) (plusp (length id)) (journal-event-p event)
                             (integerp (gethash "ts" rec)))
                        (setf (gethash id last) (cons index event))
                        (incf ignored))))))
    (let ((live nil) (cancelled nil))
      (maphash (lambda (id state)
                 (cond ((member (cdr state) '("install-commit" "resume") :test #'equal)
                        (push (cons (car state) id) live))
                       ((equal (cdr state) "install-begin")
                        (push (cons (car state) id) cancelled)))) last)
      (values (mapcar #'cdr (sort live #'< :key #'car))
              (mapcar #'cdr (sort cancelled #'< :key #'car)) ignored))))

(defun recover (&key (path (log-path)))
  "Reconcile a fresh session after component definitions/configuration are loaded.
Audit abandoned installs; reinstall only inactive :PERSIST T components in activation
order, capturing fresh inverses. Existing active/suspended session state is borrowed.
Every recovery transition is journaled to PATH even if durability is not enabled.
Returns :REINSTALLED, :CANCELLED, :SKIPPED, :ERRORS (id/condition), :IGNORED-RECORDS."
  (multiple-value-bind (live cancelled ignored) (replay-log :path path)
    (dolist (id cancelled) (log-event :install-cancelled id :path path))
    (let ((reinstalled nil) (skipped nil) (errors nil)
          (*lifecycle-hook*
            (make-journal-hook (if (eq *lifecycle-hook* *durability-hook*)
                                   *previous-hook* *lifecycle-hook*) path)))
      (dolist (id live)
        (let* ((sym (find-component-symbol id))
               (entry (and sym (gethash sym *component-registry*))))
          (if (and entry (getf entry :persist)
                   (eq (getf entry :status) :inactive)
                   (null (getf entry :state)) (null (getf entry :suspended)))
              (handler-case (progn (install-component sym) (push id reinstalled))
                (error (condition)
                  (push id skipped)
                  (push (list :component id :condition condition) errors)))
              (push id skipped))))
      (list :reinstalled (nreverse reinstalled) :cancelled cancelled
            :skipped (nreverse skipped) :errors (nreverse errors) :ignored-records ignored))))
