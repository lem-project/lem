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
   #:install-component
   #:uninstall-component
   #:toggle-component
   #:*component-registry*
   #:*lifecycle-hook*))

(in-package :yeditor-context)

;; Global registry mapping component symbols to component data
(defvar *component-registry* (make-hash-table :test #'eq)
  "Registry of defined components.")

(defvar *lifecycle-hook* nil
  "Function (EVENT COMPONENT) called on lifecycle transitions, or nil. EVENT is one
   of :install-begin :install-commit :install-abort :uninstall :suspend :resume.")

(defun notify (event component)
  (when *lifecycle-hook* (funcall *lifecycle-hook* event component)))

;; Component structure (alist) stored in the registry
;; Keys: :name :version :author :depends-on :install :activate :deactivate :config-spec :state

(defmacro define-component (name &key version author depends-on install activate deactivate config-spec persist)
  "Define a component and register it in the global registry.
   INSTALL is a function taking (component context) returning effect handles.
   ACTIVATE and DEACTIVATE are optional lifecycle functions.
   CONFIG-SPEC is optional config description."
  `(progn
     (setf (gethash ',name *component-registry*)
           (list :name ',name
                 :version ,version
                 :author ,author
                 :depends-on ',depends-on
                 :install ,install
                 :activate ,activate
                 :deactivate ,deactivate
                 :config-spec ,config-spec
                 :persist ,persist
                 :state nil
                 :suspended nil))))

(defun install-component (component &optional ctx)
  "Install COMPONENT within a transaction.
   Returns the component entry on success; signals error on failure.
   COMPONENT is a symbol already defined via DEFINE-COMPONENT."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    ;; normalize literal-quoted deps (call sites write :depends-on '(a b), which
    ;; arrives as (QUOTE (A B)) after the macro's own quoting)
    (let ((deps (getf entry :depends-on)))
      (when (and (consp deps) (eq (first deps) 'quote))
        (setf deps (second deps))
        (setf (getf entry :depends-on) deps))
      ;; dependency check (plan T3 / Cap. 16.3 topological-deferred): every :depends-on
      ;; name must be a defined component; missing deps fail the install with an
      ;; explicit message naming the dependency.
      (dolist (dep deps)
        (unless (gethash dep *component-registry*)
          (error "Dependency ~A not defined (required by component ~A)" dep component))))
    ;; fresh bindings: a component installed while another transaction is
    ;; pending must neither commit nor discard the outer effects
    (notify :install-begin component)
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (notify :install-abort component))))
      (let ((yeditor-tx:*tx-pending* nil)
            (yeditor-tx:*tx-committed* nil))
        (with-transaction
          (let ((handles (funcall (getf entry :install) component ctx)))
            (tx-commit)
            ;; only mark installed once the commit succeeded
            (setf (getf entry :state) handles)))))
    (notify :install-commit component)
    entry))

(defun uninstall-component (component)
  "Uninstall COMPONENT by executing inverse effects in LIFO order.
   Works on the stored state handles.
   Component must have been installed previously."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (run-inverses (getf entry :state))
    (setf (getf entry :state) nil
          (getf entry :suspended) nil)
    (notify :uninstall component)
    ;; also run deactivate if provided
    (let ((deactivate-fn (getf entry :deactivate)))
      (when deactivate-fn (funcall deactivate-fn component)))
    t))

(defun run-inverses (handles)
  (dolist (h (reverse handles))
    (let ((undo (cdr (assoc :inverse h))))
      (when undo (funcall undo)))))

(defun resume-handles (handles)
  "Re-run the effect of each handle in order; on error undo the ones already re-run."
  (let ((done nil))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (run-inverses (reverse done)))))
      (dolist (h handles)
        (funcall (cdr (assoc :fn h)))
        (push h done)))))

(defun toggle-component (component)
  "Suspend COMPONENT if active (undo its effects, keep handles and config), or
   resume it if suspended (re-apply the same handles). Installs it if neither."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (cond
      ((getf entry :state)
       (run-inverses (getf entry :state))
       (setf (getf entry :suspended) (getf entry :state)
             (getf entry :state) nil)
       (notify :suspend component))
      ((getf entry :suspended)
       (resume-handles (getf entry :suspended))
       (setf (getf entry :state) (getf entry :suspended)
             (getf entry :suspended) nil)
       (notify :resume component))
      (t (install-component component)))
    t))
