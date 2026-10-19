;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context/loader
  (:use :cl)
  (:import-from :yeditor-context #:*component-registry* #:install-component #:uninstall-component)
  (:export #:*applied-components* #:read-config-forms #:parse-config #:apply-config
           #:apply-config-file #:config-file-changed-p #:start-config-watcher #:stop-config-watcher))
(in-package :yeditor-context/loader)

(defvar *applied-components* (make-hash-table :test #'eq)
  "Component symbol to applied configuration; ownership is recorded in its registry entry.")

(defun read-forms (stream package)
  (let ((*read-eval* nil) (*readtable* (copy-readtable nil)) (*package* package))
    (loop :for form := (read stream nil stream) :until (eq form stream) :collect form)))

(defun read-config-forms (path &key (package *package*))
  "Stream standard s-expressions from PATH, with reader evaluation disabled."
  (with-open-file (in path :direction :input) (read-forms in package)))

(defun proper-list-p (value)
  (and (listp value) (handler-case (not (null (list-length value))) (type-error () nil))))

(defun check-options (options)
  (unless (and (proper-list-p options) (evenp (length options)))
    (error "Configuration must be an option plist: ~S" options))
  (let ((seen nil))
    (loop :for (key value) :on options :by #'cddr
          :do (unless (keywordp key) (error "Configuration option must be a keyword: ~S" key))
              (when (member key seen) (error "Duplicate configuration option: ~S" key))
              (push key seen)))
  options)

(defun component-symbol (name)
  (cond ((and name (symbolp name)) name)
        ((stringp name)
         (let ((matches nil))
           (maphash (lambda (symbol entry) (declare (ignore entry))
                      (when (or (string-equal name (symbol-name symbol))
                                (and (symbol-package symbol)
                                     (string-equal name (format nil "~A:~A" (package-name (symbol-package symbol))
                                                                (symbol-name symbol)))))
                        (push symbol matches))) *component-registry*)
           (unless (= 1 (length matches))
             (error "Component name ~S is unknown or ambiguous; use a package-qualified symbol" name))
           (first matches)))
        (t (error "Invalid component name: ~S" name))))

(defun parse-config (forms)
  "Normalize USE-COMPONENT NAME :CONFIG (OPTIONS) to (SYMBOL . OPTIONS).
   Legacy flat option plists remain accepted. Forms are data and never evaluated."
  (let ((desired nil) (*print-circle* t))
    (dolist (form forms (nreverse desired))
      (unless (and (proper-list-p form) (>= (length form) 2)
                   (symbolp (first form)) (string= (symbol-name (first form)) "USE-COMPONENT"))
        (error "Invalid config form: ~S (expected (use-component NAME :config (...)))" form))
      (let* ((sym (component-symbol (second form)))
             (tail (cddr form))
             (options (if (eq (first tail) :config)
                          (progn (unless (= 2 (length tail)) (error "Invalid :config declaration: ~S" form))
                                 (second tail)) tail)))
        (check-options options)
        (when (assoc sym desired) (error "Component ~A is declared more than once" sym))
        (push (cons sym (copy-tree options)) desired)))))

(defun validate-config (sym config)
  (check-options config)
  (let* ((entry (gethash sym *component-registry*)) (spec (getf entry :config-spec)))
    (unless entry (error "Component ~A is not defined" sym))
    (loop :for (key value) :on config :by #'cddr
          :unless (member key spec)
            :do (error "Component ~A: unknown option ~S (allowed: ~S)" sym key spec))))

(defun active-p (sym)
  (yeditor-context/reactivity:component-active-p sym))
(defun owned-p (sym)
  (eq (getf (gethash sym *component-registry*) :config-owner) *applied-components*))
(defun install-configured (sym config)
  (let ((entry (gethash sym *component-registry*)))
    (setf (getf entry :configuration) (copy-tree config)
          (getf entry :config-enabled-p) t
          (getf entry :config-owner) *applied-components*)
    (when (yeditor-context/reactivity:component-wanted-p sym) (install-component sym))
    (setf (gethash sym *applied-components*) (copy-tree config))))

(defun apply-config (desired)
  "Apply a validated component delta, preserving borrowed components and old
   configurations on replacement failure. Unchanged newer layers retain handles
   and are rebased in LIFO order. Return explicit per-component error messages."
  (let ((errors nil) (invalid nil) (changes nil) (suspended nil) (*print-circle* t))
    (labels ((report (sym condition) (push (format nil "Config ~A: ~A" sym condition) errors))
             (order-of (sym) (getf (gethash sym *component-registry*) :activation-order 0)))
      ;; Validate before removing any previous configuration.
      (dolist (item desired)
        (handler-case (validate-config (car item) (cdr item))
          (error (condition) (push (car item) invalid) (report (car item) condition))))
      (maphash (lambda (sym config)
                 (let ((want (assoc sym desired)))
                   (when (and (not (member sym invalid))
                              (or (null want) (not (equal config (cdr want)))))
                     (push sym changes)))) *applied-components*)
      (let ((yeditor-context/reactivity::*reclassifying* t))
        (unwind-protect
             (block reconcile
               (let ((owned-changes (remove-if-not (lambda (sym) (and (owned-p sym) (active-p sym)))
                                                    changes)))
                 (when owned-changes
                   (let ((oldest (reduce #'min owned-changes :key #'order-of)) (layers nil))
                     (maphash (lambda (sym entry) (declare (ignore entry))
                                (when (and (active-p sym) (>= (order-of sym) oldest)) (push sym layers)))
                              *component-registry*)
                     (dolist (sym (sort layers #'> :key #'order-of))
                       (handler-case
                           (progn (yeditor-context:toggle-component sym) (push sym suspended))
                         (error (condition) (report sym condition) (return-from reconcile nil)))))))
               (dolist (sym (sort changes #'< :key #'order-of))
                 (let ((want (assoc sym desired)) (old (gethash sym *applied-components*)))
                   (if (not (owned-p sym))
                       (if want (report sym "component is already installed outside this configuration")
                           (remhash sym *applied-components*))
                       (let* ((entry (gethash sym *component-registry*))
                              (previous-handles (getf entry :suspended))
                              (previous-status (getf entry :status)))
                         (handler-case
                             (progn
                               (setf (getf entry :config-enabled-p) nil)
                               (uninstall-component sym)
                               (setf suspended (remove sym suspended))
                               (if want (install-configured sym (cdr want))
                                   (progn (remhash sym *applied-components*)
                                          (setf (getf entry :config-owner) nil
                                                (getf entry :configuration) nil))))
                           (error (condition)
                             (report sym condition)
                             (setf (getf entry :configuration) old (getf entry :config-enabled-p) t
                                   (gethash sym *applied-components*) old)
                             (when (and (not (active-p sym)) (eq previous-status :suspended))
                               (setf (getf entry :suspended) previous-handles
                                     (getf entry :status) :suspended)
                               (pushnew sym suspended)))))))))
          ;; Resume retained layers, oldest first, against the new baselines.
          (dolist (sym (sort suspended #'< :key #'order-of))
            (when (eq (getf (gethash sym *component-registry*) :status) :suspended)
              (handler-case (yeditor-context:toggle-component sym)
                (error (condition) (report sym condition))))))
        (dolist (item desired)
          (let ((sym (car item)) (config (cdr item)))
            (unless (or (member sym invalid) (nth-value 1 (gethash sym *applied-components*)))
              (handler-case
                  (if (or (active-p sym) (eq (getf (gethash sym *component-registry*) :status) :suspended))
                      (if (and config (not (equal config (getf (gethash sym *component-registry*) :configuration))))
                          (error "component is already installed outside this configuration")
                          (setf (gethash sym *applied-components*) (copy-tree config)))
                      (install-configured sym config))
                (error (condition)
                  (when (owned-p sym)
                    (setf (getf (gethash sym *component-registry*) :config-enabled-p) nil))
                  (report sym condition)))))))
      (dolist (failure (yeditor-context/reactivity:emit-context-event :config-change))
        (report (getf failure :component) (getf failure :condition))))
    (nreverse errors)))

(defun apply-config-file (path &key (package *package*))
  "Read PATH and apply its delta. Return error strings, leaving parse failures unchanged."
  (apply-config (parse-config (read-config-forms path :package package))))

(defvar *config-file-write-date* :unseen)
(defun file-signature (path)
  (when (probe-file path) (list (file-write-date path) (uiop:read-file-string path))))
(defun config-file-changed-p (path)
  "Detect changes to PATH's mtime or content, including same-tick rewrites."
  (let ((signature (file-signature path)))
    (unless (equal signature *config-file-write-date*)
      (setf *config-file-write-date* signature) t)))

(defvar *config-watcher* nil)
(defvar *config-watcher-token* nil)
(defun show-config-error (message)
  (lem:message "~A" message))

(defun start-config-watcher (path &key (interval 2) (package *package*) (on-error #'show-config-error))
  "Poll immutable file snapshots off-thread; apply their deltas on the editor.
   ON-ERROR receives explicit messages on the editor thread (default: Lem UI)."
  (unless (and (realp interval) (plusp interval)) (error "Polling interval must be positive"))
  (stop-config-watcher)
  (let ((token (list t)))
    (setf *config-watcher-token* token
          *config-watcher*
          (sb-thread:make-thread
           (lambda ()
             (let ((previous :unseen))
               (loop :while (car token)
                     :do (handler-case
                             (let ((snapshot (file-signature path)))
                               (unless (equal previous snapshot)
                                 ;; Avoid applying a transient empty/partial overwrite.
                                 (sleep (min 0.02 (/ interval 4)))
                                 (when (equal snapshot (file-signature path))
                                  (setf previous snapshot)
                                 (let ((text (second snapshot)))
                                   (lem:send-event
                                    (lambda ()
                                      (when (car token)
                                        (dolist (message
                                                  (handler-case
                                                      (apply-config (parse-config
                                                        (if text (with-input-from-string (in text) (read-forms in package)) nil)))
                                                    (error (condition) (list (format nil "Config ~A: ~A" path condition)))))
                                          (funcall on-error message)))))))))
                           (error (condition)
                             (let ((message (format nil "Config ~A: ~A" path condition)))
                               (lem:send-event (lambda () (when (car token) (funcall on-error message)))))))
                         (sleep interval))))
           :name "yeditor-config-watcher"))))

(defun stop-config-watcher ()
  "Stop polling and invalidate queued snapshots from this watcher activation."
  (when *config-watcher-token* (setf (car *config-watcher-token*) nil))
  (when (and *config-watcher* (sb-thread:thread-alive-p *config-watcher*))
    (ignore-errors (sb-thread:terminate-thread *config-watcher*)))
  (setf *config-watcher* nil *config-watcher-token* nil))
