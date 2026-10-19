;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-sdk
  (:use :cl)
  (:import-from :yeditor-context #:effect)
  (:export #:define-component #:register-component #:find-component
           #:make-registry #:*component-registry* #:registry-log
           #:install-component #:uninstall-component #:toggle-component
           #:suspend-component #:resume-component #:effect
           #:component-name #:component-state #:component-configuration
           #:component-handles))
(in-package :yeditor-sdk)

(defvar *component-registry* nil
  "Default registry; bind a fresh MAKE-REGISTRY result for an isolated workspace.")

(defstruct (registry (:constructor make-registry ()))
  "A single-execution-context registry of reversible component definitions."
  (definitions (make-hash-table :test 'eq))
  (order nil)
  (busy-p nil))

(defstruct (component (:conc-name %component-) (:constructor make-component (name installer configuration documentation)))
  "A registered component, its retained configuration and its current transaction."
  name installer configuration documentation
  context transaction
  (state :registered))

(unless *component-registry*
  (setf *component-registry* (make-registry)))

(defun component-name (component)
  "Return COMPONENT's registered name."
  (%component-name component))

(defun component-state (component)
  "Return COMPONENT's registered, active, suspended, uninstalled or failed state."
  (%component-state component))

(defun component-configuration (component)
  "Return COMPONENT's configuration object, retained by identity across toggles."
  (%component-configuration component))

(defun find-component (name &key (registry *component-registry*))
  "Return NAME's definition/instance in REGISTRY, or NIL if unregistered."
  (gethash name (registry-definitions registry)))

(defun require-component (name registry)
  (or (find-component name :registry registry)
      (error "Unknown component: ~S" name)))

(defun component-handles (component)
  "Return a fresh list of COMPONENT's handles in registration order."
  (when (%component-transaction component)
    (yeditor-tx:transaction-handles (%component-transaction component))))

(defun registry-log (&optional (registry *component-registry*))
  "Return a snapshot of the registry's active effects, including failed inverses.
Each alist contains :component, :handle, :state and :record. Suspended components
have no entries; the component definition, configuration and handles remain.
This active view is derived from owned transactions, not a historical journal."
  (loop :for name :in (reverse (registry-order registry))
        :for component := (find-component name :registry registry)
        :when (member (%component-state component) '(:active :failed))
          :append (loop :for handle :in (component-handles component)
                        :when (member (yeditor-tx:effect-state handle) '(:active :undo-failed))
                          :collect (list (cons :component name) (cons :handle handle)
                                         (cons :state (yeditor-tx:effect-state handle))
                                         (cons :record (yeditor-tx:effect-record handle))))))

(defun register-component (name installer &key configuration documentation
                                          (registry *component-registry*))
  "Register NAME's INSTALLER without executing it. Reject duplicate definitions.
INSTALLER takes the component object and caller-supplied context and must queue
reversible effects. CONFIGURATION is retained by identity across suspend/resume."
  (check-type name symbol)
  (check-type installer function)
  (when (registry-busy-p registry)
    (error "Cannot register a component during a lifecycle transition."))
  (when (find-component name :registry registry)
    (error "Component ~S is already registered." name))
  (let ((component (make-component name installer configuration documentation)))
    (setf (gethash name (registry-definitions registry)) component)
    (push name (registry-order registry))
    component))

(defmacro define-component (name (&key configuration documentation) &body clauses)
  "Register NAME using one (:INSTALL (component context) ...) clause.
Options :CONFIGURATION and :DOCUMENTATION are evaluated at registration time."
  (unless (and (symbolp name) (= 1 (length clauses)))
    (error "DEFINE-COMPONENT requires a name and exactly one :INSTALL clause."))
  (destructuring-bind (tag bindings &body body) (first clauses)
    (unless (and (eq tag :install) (= 2 (length bindings))
                 (every #'symbolp bindings))
      (error "Expected (:INSTALL (component context) ...)."))
    `(register-component ',name (lambda ,bindings ,@body)
                         :configuration ,configuration :documentation ,documentation)))

(defun call-with-transition (registry component state function)
  (when (registry-busy-p registry)
    (error "Nested component lifecycle transitions are not allowed."))
  (setf (registry-busy-p registry) t
        (%component-state component) state)
  (let ((cause nil) (result nil))
    (unwind-protect
         (handler-case (setf result (funcall function))
           (error (condition) (setf cause condition)))
      (when (eq state (%component-state component))
        (setf (%component-state component)
              (if (and (%component-transaction component)
                       (eq :suspended (yeditor-tx:transaction-state
                                       (%component-transaction component))))
                  :suspended :failed)))
      (setf (registry-busy-p registry) nil))
    ;; Release the transition guard before reporting even an unhandled error.
    (when cause (error cause))
    result))

(defun install-component (name &key (context nil context-p) (registry *component-registry*))
  "Install NAME in a new transaction, compensating registration/commit failures.
An active install is idempotent; installing a suspended component resumes it.
An existing context cannot be replaced until the component is uninstalled."
  (when (registry-busy-p registry)
    (error "Cannot install a component during a lifecycle transition."))
  (let ((component (require-component name registry)))
    (when (member (%component-state component) '(:active :suspended))
      (when (and context-p (not (eq context (%component-context component))))
        (error "Uninstall ~S before changing its context." name))
      (return-from install-component
        (if (eq :active (%component-state component)) component
            (resume-component name :registry registry))))
    (unless (member (%component-state component) '(:registered :uninstalled))
      (error "Component ~S requires cleanup before installation." name))
    (call-with-transition
     registry component :installing
     (lambda ()
       (let ((log (yeditor-tx:make-transaction-log)) (complete-p nil) (cause nil))
         (setf (%component-transaction component) log
               (%component-context component) context)
         (unwind-protect
              (handler-case
                  (progn
                    (yeditor-tx:with-transaction (log)
                      (funcall (%component-installer component) component context))
                    (yeditor-tx:commit log)
                    (setf (%component-state component) :active complete-p t))
                (error (condition) (setf cause condition)))
           (unless (or complete-p (eq :rollback-failed (yeditor-tx:transaction-state log)))
             (yeditor-tx:rollback log :cause cause)
             (setf (%component-state component) :registered
                   (%component-transaction component) nil
                   (%component-context component) nil)))
         (when cause (error cause)))
       component))))

(defun suspend-component (name &key (registry *component-registry*))
  "Undo NAME's effects in LIFO order while retaining its definition and handles.
Retry a failed suspension explicitly; partial failures remain visible in the log."
  (let ((component (require-component name registry)))
    (when (registry-busy-p registry)
      (error "Cannot suspend during a lifecycle transition."))
    (when (eq :suspended (%component-state component))
      (return-from suspend-component component))
    (unless (or (eq :active (%component-state component))
                (and (eq :failed (%component-state component))
                     (eq :suspend-failed (yeditor-tx:transaction-state
                                         (%component-transaction component)))))
      (error "Component ~S is not active or awaiting suspension recovery." name))
    (call-with-transition registry component :suspending
                          (lambda ()
                            (yeditor-tx:suspend (%component-transaction component))
                            (setf (%component-state component) :suspended)
                            component))))

(defun resume-component (name &key (registry *component-registry*))
  "Reapply NAME's suspended installers with the same handles and captured context."
  (let ((component (require-component name registry)))
    (when (registry-busy-p registry)
      (error "Cannot resume during a lifecycle transition."))
    (when (eq :active (%component-state component))
      (return-from resume-component component))
    (unless (eq :suspended (%component-state component))
      (error "Component ~S is not suspended." name))
    (call-with-transition registry component :resuming
                          (lambda ()
                            (yeditor-tx:resume (%component-transaction component))
                            (setf (%component-state component) :active)
                            component))))

(defun uninstall-component (name &key (registry *component-registry*))
  "Roll back NAME and release its runtime handles/context, retaining its definition.
If an inverse fails, retain the failed instance for a later explicit cleanup retry."
  (let ((component (require-component name registry)))
    (when (registry-busy-p registry)
      (error "Cannot uninstall during a lifecycle transition."))
    (when (eq :uninstalled (%component-state component))
      (return-from uninstall-component component))
    (call-with-transition registry component :uninstalling
                          (lambda ()
                            (when (%component-transaction component)
                              (yeditor-tx:rollback (%component-transaction component)))
                            (setf (%component-state component) :uninstalled
                                  (%component-transaction component) nil
                                  (%component-context component) nil)
                            component))))

(defun toggle-component (name &key context (registry *component-registry*))
  "Suspend an active NAME or resume it; install a registered/uninstalled NAME.
CONTEXT is used only for a new installation. Failed instances require cleanup."
  (let ((component (require-component name registry)))
    (case (%component-state component)
      (:active (suspend-component name :registry registry))
      (:suspended (resume-component name :registry registry))
      ((:registered :uninstalled) (install-component name :context context :registry registry))
      (otherwise (error "Component ~S requires explicit cleanup." name)))))
