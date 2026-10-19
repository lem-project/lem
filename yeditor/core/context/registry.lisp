;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context
  (:use :cl)
  (:import-from :yeditor-tx
   #:install-effect
   #:tx-commit
   #:tx-rollback
   #:with-transaction)
  (:export
   #:define-component
   #:register-component
   #:unregister-component
   #:install-component
   #:uninstall-component
   #:toggle-component
   #:*component-registry*
   #:*lifecycle-hook*))

(in-package :yeditor-context)

;; Global registry mapping component symbols to component data
(defvar *component-registry* (make-hash-table :test #'eq)
  "Registry of defined components.")

(defvar *activation-sequence* 0)

(defvar *lifecycle-hook* nil
  "Function (EVENT COMPONENT) called on lifecycle transitions, or nil. EVENT is one
   of :install-begin :install-commit :install-abort :uninstall :suspend :resume.")

(defun notify (event component)
  (when *lifecycle-hook* (funcall *lifecycle-hook* event component)))

;; Component structure (alist) stored in the registry
;; Keys: :name :version :author :depends-on :install :activate :deactivate :config-spec :state

(defun register-component (name &key version author depends-on install activate deactivate
                                      config-spec persist (requires nil requires-p))
  "Register component NAME (a symbol) in the global registry, replacing any previous
   definition. Functional form of DEFINE-COMPONENT for components created at runtime."
  (setf (gethash name *component-registry*)
        (list :name name
              :version version
              :author author
              :depends-on depends-on
              :install install
              :activate activate
              :deactivate deactivate
              :config-spec config-spec
              :persist persist
              :state nil
              :suspended nil
              :status :inactive))
  (when requires-p
    (setf (getf (gethash name *component-registry*) :requires) requires))
  name)

(defun unregister-component (name)
  "Remove NAME from the registry. Signals an error if it is installed or suspended."
  (let ((entry (gethash name *component-registry*)))
    (when (and entry (or (getf entry :state) (getf entry :suspended)
                           (member (getf entry :status) '(:active :suspended))))
      (error "Component ~A is installed; uninstall it before unregistering" name))
    (remhash name *component-registry*)))

(defmacro define-component (name &key version author depends-on install activate deactivate config-spec persist (requires nil requires-p))
  "Define a component and register it in the global registry.
   INSTALL is a function taking (component context) returning effect handles.
   ACTIVATE and DEACTIVATE are optional lifecycle functions.
   CONFIG-SPEC is optional config description. REQUIRES is a predicate plist
   classified by yeditor-context-reactivity (for example (:buffer-mode MODE))."
  `(register-component ',name
                       :version ,version
                       :author ,author
                       :depends-on ',depends-on
                       :install ,install
                       :activate ,activate
                       :deactivate ,deactivate
                       :config-spec ,config-spec
                       :persist ,persist
                       ,@(when requires-p `(:requires ,requires))))

(defun installed-p (entry)
  (or (eq (getf entry :status) :active) (getf entry :state)))

(defun install-component (component &optional ctx)
  "Install COMPONENT in an isolated subtransaction, including ACTIVATE.
   Repeated installation is idempotent, including components with no effects."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (when (installed-p entry) (return-from install-component entry))
    (when (eq (getf entry :status) :suspended)
      (toggle-component component)
      (return-from install-component entry))
    (let ((deps (getf entry :depends-on)))
      (when (and (consp deps) (eq (first deps) 'quote)) (setf deps (second deps)))
      (dolist (dep deps)
        (unless (gethash dep *component-registry*)
          (error "Dependency ~A not defined (required by component ~A)" dep component))))
    (let ((yeditor-tx:*tx-pending* nil)
          (yeditor-tx:*tx-committed* nil))
      (notify :install-begin component)
      (handler-case
          (with-transaction
            (let ((handles (funcall (getf entry :install) component ctx)))
              (tx-commit)
              (when (getf entry :activate) (funcall (getf entry :activate) component))
              (setf (getf entry :state) (or handles yeditor-tx:*tx-committed*)
                    (getf entry :status) :active
                    (getf entry :activation-order) (incf *activation-sequence*))
              (notify :install-commit component)))
        (error (condition)
          (tx-rollback)
          (setf (getf entry :state) nil (getf entry :status) :inactive)
          (notify :install-abort component)
          (error condition))))
    entry))

(defun run-inverses (handles)
  "Undo in LIFO order; if an inverse fails, reapply completed undos in order.
   Each effect must provide atomic apply/undo operations; compensation errors
   propagate rather than pretending the resource state was restored."
  (let ((undone nil))
    (handler-case
        (dolist (handle (reverse handles))
          (let ((undo (cdr (assoc :inverse handle))))
            (when undo (funcall undo) (push handle undone))))
      (error (condition)
        (dolist (handle undone) (funcall (cdr (assoc :fn handle))))
        (error condition)))))

(defun resume-handles (handles)
  "Reapply handles in order; undo completed applications if one fails."
  (let ((done nil))
    (handler-case
        (dolist (handle handles)
          (funcall (cdr (assoc :fn handle)))
          (push handle done))
      (error (condition)
        (run-inverses (reverse done))
        (error condition)))))

(defun uninstall-component (component)
  "Remove COMPONENT atomically, restoring completed undos on transition failure."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (unless (or (installed-p entry) (eq (getf entry :status) :suspended)
                (getf entry :suspended))
      (return-from uninstall-component t))
    (let ((handles (getf entry :state))
          (yeditor-tx:*tx-pending* nil)
          (yeditor-tx:*tx-committed* nil))
      (run-inverses handles)
      (handler-case
          (progn
            (when (getf entry :deactivate) (funcall (getf entry :deactivate) component))
            (notify :uninstall component))
        (error (condition) (resume-handles handles) (error condition)))
      (setf (getf entry :state) nil (getf entry :suspended) nil
            (getf entry :status) :inactive))
    t))

(defun toggle-component (component)
  "Suspend or resume COMPONENT atomically, retaining its effect handles."
  (let ((entry (gethash component *component-registry*))
        (yeditor-tx:*tx-pending* nil)
        (yeditor-tx:*tx-committed* nil))
    (unless entry (error "Component ~A not defined" component))
    (cond
      ((installed-p entry)
       (run-inverses (getf entry :state))
       (setf (getf entry :suspended) (getf entry :state)
             (getf entry :state) nil (getf entry :status) :suspended)
       (handler-case (notify :suspend component)
         (error (condition)
           (resume-handles (getf entry :suspended))
           (setf (getf entry :state) (getf entry :suspended)
                 (getf entry :suspended) nil (getf entry :status) :active)
           (error condition))))
      ((or (eq (getf entry :status) :suspended) (getf entry :suspended))
       (resume-handles (getf entry :suspended))
       (setf (getf entry :state) (getf entry :suspended)
             (getf entry :suspended) nil (getf entry :status) :active
             (getf entry :activation-order) (incf *activation-sequence*))
       (handler-case (notify :resume component)
         (error (condition)
           (run-inverses (getf entry :state))
           (setf (getf entry :suspended) (getf entry :state)
                 (getf entry :state) nil (getf entry :status) :suspended)
           (error condition))))
      (t (install-component component)))
    t))
