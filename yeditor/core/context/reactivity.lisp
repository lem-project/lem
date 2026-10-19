;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

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
   #:set-work-predicate
   #:update-context
   #:update-contexts
   #:context-value
   #:component-active-p
   #:component-wanted-p
   #:context-convergence-error
   #:*max-reclassification-passes*))
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
;;   :project-root   — pathname: (equal current-project-root value)
;;   :project-marker — list of file names: the project root contains any of them
;;   :any            — list of coeffect plists: satisfied if any one is

(defvar *coeffect-registry* (make-hash-table :test #'eq)
  "Maps component symbol → coeffect plist.")

(defun define-coeffects (component-sym coeffects)
  "Register COEFFECTS (a plist) for COMPONENT-SYM.
   Called by components that declare context:requires at define time."
  (setf (gethash component-sym *coeffect-registry*) coeffects))

(defun component-coeffects (component-sym)
  "Return component predicates and a second value indicating a declaration.
   :REQUIRES in DEFINE-COMPONENT takes precedence over DEFINE-COEFFECTS."
  (let ((entry (gethash component-sym *component-registry*)))
    (multiple-value-bind (key value) (get-properties entry '(:requires))
      (if key (values value t) (gethash component-sym *coeffect-registry*)))))

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
                                :window-width 0
                                :project-root nil)
  "Snapshot of the observable context used for coeffect classification.")

(defun context-value (key)
  (getf *current-context* key))

(defun update-contexts (&rest keys-and-values)
  "Update several context slots at once, then reclassify a single time (so a
   component is not deactivated and reactivated in between)."
  (unless (evenp (length keys-and-values)) (error "Context updates must be a plist"))
  (let ((previous *current-context*))
    (setf *current-context* (copy-list previous))
    (loop :for (key value) :on keys-and-values :by #'cddr
          :do (setf (getf *current-context* key) value))
    (unless (and *reclassifying* (equal previous *current-context*))
      (reclassify-all))))

(defun update-context (key value)
  "Update one slot of *current-context* and reclassify all components."
  (update-contexts key value))

;;;; ── Coeffect satisfaction check ────────────────────────────────────────

(defun coeffect-satisfied-p (coeffects)
  "Return true if every predicate in COEFFECTS is satisfied by *current-context*
   and the work-cycle predicates."
  (unless (and (listp coeffects) (evenp (length coeffects)))
    (error "Coeffects must be a predicate plist: ~S" coeffects))
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
                   (if (getf *current-context* :server-statuses)
                       (eq (cdr (assoc value (getf *current-context* :server-statuses) :test #'equal))
                           :online)
                       (equal (getf *current-context* :server-online) value)))
                  (:window-width
                   (>= (getf *current-context* :window-width 0) value))
                  (:agent-tx-active
                   (eq *agent-tx-active* value))
                  (:eval-overlay-active
                   (eq *eval-overlay-active* value))
                  (:diagnostics-critical
                   (eq *diagnostics-critical* value))
                  (:project-root
                   (equal (getf *current-context* :project-root) value))
                  (:project-marker
                   ;; satisfied when the current project root contains any of the files
                   (let ((root (getf *current-context* :project-root)))
                     (and root (some (lambda (marker) (probe-file (merge-pathnames marker root)))
                                     value))))
                  (:any
                   ;; disjunction: VALUE is a list of coeffect plists
                   (some #'coeffect-satisfied-p value))
                  (otherwise (error "Unknown coeffect predicate: ~S" key)))))

;;;; ── Reclassification ────────────────────────────────────────────────────

(defun component-active-p (sym)
  "True when SYM is active, including a component with no effect handles."
  (let ((entry (gethash sym *component-registry*)))
    (and entry (or (eq (getf entry :status) :active) (not (null (getf entry :state)))))))

(defun component-wanted-p (sym)
  "True when SYM is enabled by configuration and its declared predicates match."
  (let ((entry (gethash sym *component-registry*)))
    (and entry (getf entry :config-enabled-p t)
         (coeffect-satisfied-p (component-coeffects sym)))))

(defvar *reclassifying* nil "True while a reclassification pass runs.")
(defvar *reclassify-pending* nil "A nested request schedules another pass.")
(defparameter *max-reclassification-passes* 100
  "Maximum feedback passes before reporting a non-converging context.")
(define-condition context-convergence-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Reactive context did not converge; component callbacks keep changing it" stream))))

(defun reclassify-once ()
  (let ((names nil) (active nil) (removals nil) (suspended nil) (failures nil))
    ;; Snapshot names: callbacks may mutate the registry during a transition.
    (maphash (lambda (sym entry) (declare (ignore entry)) (push sym names)) *component-registry*)
    (setf names (nreverse names))
    (dolist (sym names)
      (multiple-value-bind (coeffects declared-p) (component-coeffects sym)
        (when (and declared-p (component-active-p sym))
          (push sym active)
          (unless (component-wanted-p sym) (push sym removals)))))
    (labels ((order-of (sym) (getf (gethash sym *component-registry*) :activation-order 0))
             (transition (sym function)
               (handler-case (progn (funcall function sym) t)
                 (error (condition)
                   (push (list :component sym :condition condition) failures)
                   (warn "Reactivity: transition failed for ~A: ~A" sym condition)
                   nil))))
      (when removals
        (let ((oldest (reduce #'min removals :key #'order-of)))
          ;; Unwind older removals only after newer layers have been undone.
          ;; Retained layers keep their handles and recapture their new baseline
          ;; on resume, so they never restore a removed component's stale value.
          (dolist (sym (sort active #'> :key #'order-of))
            (when (>= (order-of sym) oldest)
              (if (member sym removals)
                  (unless (transition sym #'uninstall-component) (return))
                  (if (transition sym #'yeditor-context:toggle-component)
                      (push sym suspended)
                      (return)))))))
      (dolist (sym suspended)
        (if (component-wanted-p sym)
            (transition sym #'yeditor-context:toggle-component)
            (transition sym #'uninstall-component)))
      (dolist (sym names)
        (multiple-value-bind (coeffects declared-p) (component-coeffects sym)
          (when (and declared-p (gethash sym *component-registry*)
                     (not (component-active-p sym)) (component-wanted-p sym))
            (transition sym (lambda (name) (install-component name *current-context*)))))))
    (nreverse failures)))

(defun reclassify-all ()
  "Reconcile declared components in isolated subtransactions.
   Return transition failures. Nested requests are deferred; feedback that never
   settles signals CONTEXT-CONVERGENCE-ERROR rather than silently stopping."
  (if *reclassifying*
      (progn (setf *reclassify-pending* t) nil)
      (let ((*reclassifying* t) (failures nil))
        (loop :repeat *max-reclassification-passes*
              :do (setf *reclassify-pending* nil)
                  (setf failures (nconc failures (reclassify-once)))
              :unless *reclassify-pending* :do (return-from reclassify-all failures))
        (error 'context-convergence-error))))

(defun emit-context-event (event &rest args)
  "Publish buffer, project, server, config or window context changes.
   Buffer events carry the buffer; mode changes also carry the new mode.
   Background mode/close events do not replace the observed buffer. A NIL buffer
   on mode/close events explicitly updates/clears the synthetic context.
   Server status is tracked per key, independently of other servers."
  (ecase event
    (:buffer-open
     (let* ((buffer (first args)) (file (and buffer (lem:buffer-filename buffer))))
       (update-contexts :buffer buffer
                        :buffer-mode (when buffer (lem:buffer-major-mode buffer))
                        :file-type (when file (pathname-type file)))))
    (:buffer-mode-change
     (let ((buffer (first args)))
       (when (or (null buffer) (eq buffer (context-value :buffer)))
         (update-context :buffer-mode (second args)))))
    (:buffer-close
     (when (or (null (first args)) (eq (first args) (context-value :buffer)))
       (update-contexts :buffer nil :buffer-mode nil :file-type nil
                       :project-root nil :project-type nil :remote-p nil)))
    (:project-root-change (update-context :project-root (first args)))
    (:project-change (update-context :project-type (first args)))
    (:file-type-change (update-context :file-type (first args)))
    (:remote-change (update-context :remote-p (first args)))
    (:server-status
     (destructuring-bind (key status) args
       (unless (member status '(:online :offline)) (error "Invalid server status: ~S" status))
       (update-contexts :server-statuses
                       (acons key status (remove key (context-value :server-statuses) :key #'car :test #'equal))
                       :server-online (when (eq status :online) key))))
    (:config-change (reclassify-all))
    (:window-resize (update-context :window-width (first args)))))
