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
   #:*component-registry*))

(in-package :yeditor-context)

;; Global registry mapping component symbols to component data
(defvar *component-registry* (make-hash-table :test #'eq)
  "Registry of defined components.")

;; Component structure (alist) stored in the registry
;; Keys: :name :version :author :depends-on :install :activate :deactivate :config-spec :state

(defmacro define-component (name &key version author depends-on install activate deactivate config-spec)
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
                 :state nil))))

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
    (with-transaction
      (let* ((install-fn (getf entry :install))
             (handles (funcall install-fn component ctx)))
        ;; store handles in state for later uninstall/toggle
        (setf (getf entry :state) handles)
        ;; commit transaction (effects already run inside install-fn)
        (tx-commit)
        entry))))

(defun uninstall-component (component)
  "Uninstall COMPONENT by executing inverse effects in LIFO order.
   Works on the stored state handles.
   Component must have been installed previously."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (let ((handles (getf entry :state)))
      (when handles
        ;; run inverses in reverse order
        (dolist (h (reverse handles))
          (let ((undo (cdr (assoc :inverse h))))
            (when undo (funcall undo))))
        (setf (getf entry :state) nil)))
    ;; also run deactivate if provided
    (let ((deactivate-fn (getf entry :deactivate)))
      (when deactivate-fn (funcall deactivate-fn component)))
    t))

(defun toggle-component (component)
  "Suspend or resume COMPONENT.
   If currently active (state non-nil), suspend (uninstall). Otherwise, resume (install)."
  (let ((entry (gethash component *component-registry*)))
    (unless entry (error "Component ~A not defined" component))
    (if (getf entry :state)
        (progn
          (uninstall-component component)
          (setf (getf entry :state) nil))
        (progn
          (install-component component)
          (setf (getf entry :state) (getf entry :state))))
    t))
