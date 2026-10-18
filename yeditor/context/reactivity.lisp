(defpackage :yeditor-context/reactivity
  (:use :cl)
  (:import-from :yeditor-context
   #:*component-registry*
   #:install-component
   #:uninstall-component)
  (:export
   ;; coeffect declaration
   #:define-coeffects
   #:component-coeffects
   ;; context-event dispatch
   #:emit-context-event
   ;; work-cycle predicates (Emenda 5)
   #:*agent-tx-active*
   #:*eval-overlay-active*
   #:*diagnostics-critical*
   #:set-work-predicate))
(in-package :yeditor-context/reactivity)

;;;; ── Coeffect registry ───────────────────────────────────────────────────
;;
;; Associates a component symbol with a plist of predicates that must ALL
;; be truthy for the component to be active.  Supported keys (Cap. 9 T7):
;;   :buffer-mode    — symbol: (eq current-major-mode value)
;;   :project-type   — symbol: (eq current-project-type value)
;;   :file-type      — string: (equal current-file-extension value)
;;   :remote-p       — boolean: (eq remote-p value)
;;   :server-online  — keyword: matching server must be registered as :online
;;   :window-width   — integer: (>= display-width value)
;;   :agent-tx-active     — boolean (Emenda 5)
;;   :eval-overlay-active — boolean (Emenda 5)
;;   :diagnostics-critical — boolean (Emenda 5)

(defvar *coeffect-registry* (make-hash-table :test #'eq)
  "Maps component symbol → coeffect plist.")

(defun define-coeffects (component-sym coeffects)
  "Register COEFFECTS (a plist) for COMPONENT-SYM.
   Called by components that declare context:requires at define time."
  (setf (gethash component-sym *coeffect-registry*) coeffects))

(defun component-coeffects (component-sym)
  "Return the coeffect plist for COMPONENT-SYM, or nil."
  (gethash component-sym *coeffect-registry*))

;;;; ── Work-cycle predicates (Emenda 5) ───────────────────────────────────
;;
;; Computed by L2; updated by le-agent-host (T17/T22) and Eval-in-place (T20).

(defvar *agent-tx-active* nil
  "True while any worker has an effect-transaction open and pending approval.")

(defvar *eval-overlay-active* nil
  "True while there are active Eval-in-place ghost overlays in any buffer.")

(defvar *diagnostics-critical* nil
  "True while a test/build failed after a human or agent action.")

(defun set-work-predicate (key value)
  "Set a work-cycle predicate.  KEY is one of :agent-tx-active,
   :eval-overlay-active, :diagnostics-critical; VALUE is boolean.
   Re-classifies all components against the updated context."
  (ecase key
    (:agent-tx-active     (setf *agent-tx-active* value))
    (:eval-overlay-active (setf *eval-overlay-active* value))
    (:diagnostics-critical (setf *diagnostics-critical* value)))
  (reclassify-all))

;;;; ── Context state (current observable context) ─────────────────────────

(defvar *current-context* (list :buffer-mode nil
                                :project-type nil
                                :file-type nil
                                :remote-p nil
                                :server-online nil
                                :window-width 0)
  "Snapshot of the observable context used for coeffect classification.")

(defun update-context (key value)
  "Update one slot of *current-context* and reclassify all components."
  (setf (getf *current-context* key) value)
  (reclassify-all))

;;;; ── Coeffect satisfaction check ────────────────────────────────────────

(defun coeffect-satisfied-p (coeffects)
  "Return true if every predicate in COEFFECTS is satisfied by *current-context*
   and the work-cycle predicates."
  (loop :for (key value) :on coeffects :by #'cddr
        :always (case key
                  (:buffer-mode
                   (eq (getf *current-context* :buffer-mode) value))
                  (:project-type
                   (eq (getf *current-context* :project-type) value))
                  (:file-type
                   (equal (getf *current-context* :file-type) value))
                  (:remote-p
                   (eq (getf *current-context* :remote-p) value))
                  (:server-online
                   (eq (getf *current-context* :server-online) value))
                  (:window-width
                   (>= (getf *current-context* :window-width 0) value))
                  (:agent-tx-active
                   (eq *agent-tx-active* value))
                  (:eval-overlay-active
                   (eq *eval-overlay-active* value))
                  (:diagnostics-critical
                   (eq *diagnostics-critical* value))
                  (t nil))))

;;;; ── Reclassification ────────────────────────────────────────────────────

(defun component-active-p (sym)
  "True if the component has non-nil :state (installed and not suspended)."
  (let ((entry (gethash sym *component-registry*)))
    (and entry (getf entry :state))))

(defun reclassify-all ()
  "Check every component with declared coeffects against the current context.
   Activate those whose coeffects are satisfied and are not yet active;
   deactivate those whose coeffects are no longer satisfied.
   Each transition is an install or uninstall sub-transaction (never partial)."
  (maphash
   (lambda (sym coeffects)
     (let ((satisfied (coeffect-satisfied-p coeffects))
           (active    (component-active-p sym)))
       (cond
         ((and satisfied (not active))
          ;; coeffect became true and component is inactive → activate
          (handler-case (install-component sym)
            (error (e)
              (warn "Reactivity: failed to activate ~A: ~A" sym e))))
         ((and (not satisfied) active)
          ;; coeffect became false and component is active → deactivate
          (handler-case (uninstall-component sym)
            (error (e)
              (warn "Reactivity: failed to deactivate ~A: ~A" sym e)))))))
   *coeffect-registry*))

;;;; ── Context-event dispatch ─────────────────────────────────────────────

(defun emit-context-event (event &rest args)
  "Dispatch a context lifecycle event and update *current-context*.
   Supported EVENTs and their ARGS:
     :buffer-open         (buffer)
     :buffer-mode-change  (buffer new-mode)
     :project-change      (project-type)
     :file-type-change    (extension)
     :server-status       (key :online/:offline)
     :config-change       ()   — triggers reclassify only
     :window-resize       (width)"
  (ecase event
    (:buffer-open
     (let ((buffer (first args)))
       (update-context :buffer-mode (when buffer (lem:buffer-major-mode buffer)))))
    (:buffer-mode-change
     (let ((new-mode (second args)))
       (update-context :buffer-mode new-mode)))
    (:project-change
     (update-context :project-type (first args)))
    (:file-type-change
     (update-context :file-type (first args)))
    (:server-status
     (update-context :server-online (when (eq (second args) :online) (first args))))
    (:config-change
     (reclassify-all))
    (:window-resize
     (update-context :window-width (first args)))))
