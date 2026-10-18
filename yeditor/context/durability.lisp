;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

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

(defun default-effects-log-path ()
  (uiop:xdg-data-home "yeditor/effects.log"))

(defvar *effects-log-path* nil
  "Path of the append-only JSONL effects log; nil means default-effects-log-path.")

(defvar *log-lock* (sb-thread:make-mutex :name "yeditor-effects-log"))

(defun log-path () (or *effects-log-path* (default-effects-log-path)))

(defun component-id (component)
  (format nil "~A::~A" (package-name (symbol-package component)) (symbol-name component)))

(defun find-component-symbol (id)
  (let ((sep (search "::" id)))
    (when sep
      (let ((pkg (find-package (subseq id 0 sep))))
        (when pkg (find-symbol (subseq id (+ sep 2)) pkg))))))

(defun log-event (event component &key (path (log-path)))
  "Append one JSON line {ts, event, component} to PATH and flush it."
  (let ((record (make-hash-table :test #'equal)))
    (setf (gethash "ts" record) (get-universal-time)
          (gethash "event" record) (string-downcase (symbol-name event))
          (gethash "component" record) (if (symbolp component) (component-id component) component))
    (sb-thread:with-mutex (*log-lock*)
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :append
                                :if-does-not-exist :create :external-format :utf-8)
        (yason:encode record out)
        (terpri out)
        (finish-output out)))))

(defun enable-durability (&key path)
  "Record every component lifecycle transition in the effects log."
  (when path (setf *effects-log-path* path))
  (setf *lifecycle-hook* (lambda (event component) (log-event event component))))

(defun disable-durability ()
  (setf *lifecycle-hook* nil))

(defun replay-log (&key (path (log-path)))
  "Fold the log into the state at the last write. Returns two lists of component ids:
   LIVE (committed and neither uninstalled nor suspended) and CANCELLED (an
   install-begin never followed by commit or abort: the process died mid-install).
   Unparseable lines (a torn final write) are skipped."
  (let ((last (make-hash-table :test #'equal))
        (order nil))
    (when (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (loop :for line := (read-line in nil)
              :while line
              :for rec := (ignore-errors (yason:parse line))
              :when (hash-table-p rec)
                :do (let ((id (gethash "component" rec)))
                      (unless (nth-value 1 (gethash id last)) (push id order))
                      (setf (gethash id last) (gethash "event" rec))))))
    (let ((live nil) (cancelled nil))
      (dolist (id (nreverse order))
        (let ((event (gethash id last)))
          (cond ((member event '("install-commit" "resume") :test #'string=) (push id live))
                ((string= event "install-begin") (push id cancelled)))))
      (values (nreverse live) (nreverse cancelled)))))

(defun recover (&key (path (log-path)))
  "Boot-time reconciliation. Installs that never committed are recorded as
   install-cancelled (never re-run: nothing is installed without its inverse).
   Live components declared with :persist t are re-installed. Returns a plist
   :reinstalled :cancelled :skipped (ids)."
  (multiple-value-bind (live cancelled) (replay-log :path path)
    (dolist (id cancelled) (log-event :install-cancelled id :path path))
    (let ((reinstalled nil) (skipped nil))
      (dolist (id live)
        (let* ((sym (find-component-symbol id))
               (entry (and sym (gethash sym *component-registry*))))
          (if (and entry (getf entry :persist) (null (getf entry :state)))
              (handler-case (progn (install-component sym) (push id reinstalled))
                (error () (push id skipped)))
              (push id skipped))))
      (list :reinstalled (nreverse reinstalled)
            :cancelled cancelled
            :skipped (nreverse skipped)))))
