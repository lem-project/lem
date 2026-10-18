;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context/loader
  (:use :cl)
  (:import-from :yeditor-context
   #:*component-registry*
   #:install-component
   #:uninstall-component)
  (:export
   #:*applied-components*
   #:read-config-forms
   #:parse-config
   #:apply-config
   #:apply-config-file
   #:config-file-changed-p
   #:start-config-watcher
   #:stop-config-watcher))
(in-package :yeditor-context/loader)

(defvar *applied-components* (make-hash-table :test #'eq)
  "Component symbol -> config plist for components installed from the config file.")

(defun read-config-forms (path &key (package *package*))
  "Read every top-level form of the config file at PATH with *read-eval* disabled."
  (let ((*read-eval* nil)
        (*package* package))
    (with-open-file (in path :direction :input)
      (loop :for form := (read in nil in)
            :until (eq form in)
            :collect form))))

(defun parse-config (forms)
  "Turn FORMS of the shape (use-component SYMBOL &rest CONFIG-PLIST) into an alist
   (SYMBOL . CONFIG-PLIST). Signals an error on any other form or a duplicate."
  (let ((desired nil))
    (dolist (form forms (nreverse desired))
      (unless (and (consp form) (symbolp (first form))
                 (string= (symbol-name (first form)) "USE-COMPONENT")
                 (symbolp (second form)))
        (error "Invalid config form: ~S (expected (use-component SYMBOL &key ...))" form))
      (let ((sym (second form)))
        (when (assoc sym desired)
          (error "Component ~A is declared more than once in the config" sym))
        (push (cons sym (cddr form)) desired)))))

(defun validate-config (sym config)
  "Reject CONFIG keys not declared in the component's :config-spec (when declared)."
  (let* ((entry (gethash sym *component-registry*))
         (spec (getf entry :config-spec)))
    (unless entry
      (error "Component ~A is not defined" sym))
    (when spec
      (loop :for key :in (loop :for (k) :on config :by #'cddr :collect k)
            :unless (member key spec)
              :do (error "Component ~A: unknown option ~S (allowed: ~{~S~^ ~})"
                         sym key spec)))))

(defun apply-config (desired)
  "Reconcile *applied-components* with DESIRED (alist from parse-config).
   Components that left the config or whose options changed are uninstalled first;
   new or changed ones are installed. Each step is independent; failures are
   collected and returned as a list of error messages instead of aborting."
  (let ((errors nil))
    (flet ((report (fmt &rest args)
             (push (apply #'format nil fmt args) errors)))
      (loop :for sym :being :the :hash-keys :of *applied-components* :using (:hash-value config)
            :for want := (assoc sym desired)
            :when (or (null want) (not (equal (cdr want) config)))
              :do (handler-case
                      (progn (uninstall-component sym)
                             (remhash sym *applied-components*))
                    (error (e) (report "uninstall ~A: ~A" sym e))))
      (loop :for (sym . config) :in desired
            :unless (nth-value 1 (gethash sym *applied-components*))
              :do (handler-case
                      (progn (validate-config sym config)
                             (install-component sym)
                             (setf (gethash sym *applied-components*) config))
                    (error (e) (report "install ~A: ~A" sym e)))))
    (nreverse errors)))

(defun apply-config-file (path &key (package *package*))
  "Read the config at PATH and reconcile the applied components with it.
   Returns the list of error messages (empty on full success)."
  (apply-config (parse-config (read-config-forms path :package package))))

(defvar *config-file-write-date* nil)

(defun config-file-changed-p (path)
  "True when PATH's write date differs from the last one recorded by this function."
  (let ((date (ignore-errors (file-write-date path))))
    (unless (eql date *config-file-write-date*)
      (setf *config-file-write-date* date)
      t)))

(defvar *config-watcher* nil)

(defun start-config-watcher (path &key (interval 2) (package *package*))
  "Poll PATH every INTERVAL seconds in a dedicated thread and reapply it when it changes (HMR)."
  (stop-config-watcher)
  (setf *config-watcher*
        (sb-thread:make-thread
         (lambda ()
           (loop
             (when (config-file-changed-p path)
               (dolist (msg (handler-case (apply-config-file path :package package)
                              (error (e) (list (format nil "config: ~A" e)))))
                 (warn "Config reload: ~A" msg)))
             (sleep interval)))
         :name "yeditor-config-watcher")))

(defun stop-config-watcher ()
  (when (and *config-watcher* (sb-thread:thread-alive-p *config-watcher*))
    (sb-thread:terminate-thread *config-watcher*))
  (setf *config-watcher* nil))
