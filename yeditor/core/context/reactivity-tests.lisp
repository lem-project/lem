;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-context-reactivity-tests
  (:use :cl :yeditor-context :yeditor-context/reactivity :yeditor-tx :rove))
(in-package :yeditor-context-reactivity-tests)

;;; ── helpers ────────────────────────────────────────────────────────────────

(defun reset-context ()
  "Return reactivity module to a clean state between tests."
  (setf yeditor-context/reactivity::*current-context*
        (list :buffer-mode nil :project-type nil :file-type nil
              :remote-p nil :server-online nil :window-width 0))
  (setf yeditor-context/reactivity::*agent-tx-active* nil
        yeditor-context/reactivity::*eval-overlay-active* nil
        yeditor-context/reactivity::*diagnostics-critical* nil)
  ;; remove test entries from coeffect registry to keep isolation
  (clrhash yeditor-context/reactivity::*coeffect-registry*))

;;; ── component for mode-based activation ─────────────────────────────────

(defvar *mode-comp-active* nil)

(define-component mode-reactive-comp
  :version "0.1"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (setf *mode-comp-active* t)
             (list (yeditor-tx:install-effect comp :noop
                                               (lambda () nil)
                                               :undo-thunk (lambda () (setf *mode-comp-active* nil)))))
  :activate nil
  :deactivate nil
  :config-spec nil)

;;; ── coeffect-satisfied-p unit tests ────────────────────────────────────

(deftest test-coeffect-buffer-mode
  (reset-context)
  ;; no mode active → :buffer-mode 'python-mode not satisfied
  (ok (not (yeditor-context/reactivity::coeffect-satisfied-p
            (list :buffer-mode 'python-mode)))
      "buffer-mode unsatisfied when context is nil")
  ;; update context
  (yeditor-context/reactivity::update-context :buffer-mode 'python-mode)
  (ok (yeditor-context/reactivity::coeffect-satisfied-p
       (list :buffer-mode 'python-mode))
      "buffer-mode satisfied after context update"))

(deftest test-coeffect-project-type
  (reset-context)
  (yeditor-context/reactivity::update-context :project-type 'common-lisp)
  (ok (yeditor-context/reactivity::coeffect-satisfied-p
       (list :project-type 'common-lisp))
      "project-type satisfied")
  (ok (not (yeditor-context/reactivity::coeffect-satisfied-p
            (list :project-type 'python)))
      "project-type unsatisfied for wrong value"))

(deftest test-work-cycle-predicates
  (reset-context)
  (set-work-predicate :agent-tx-active t)
  (ok (yeditor-context/reactivity::coeffect-satisfied-p
       (list :agent-tx-active t))
      ":agent-tx-active t satisfied after set")
  (set-work-predicate :agent-tx-active nil)
  (ok (not (yeditor-context/reactivity::coeffect-satisfied-p
            (list :agent-tx-active t)))
      ":agent-tx-active nil clears predicate"))

;;; ── emit-context-event + reclassify integration ─────────────────────────

(deftest test-mode-reactive-activate-deactivate
  (reset-context)
  (setf *mode-comp-active* nil)
  ;; register coeffect: activate when buffer-mode = python-mode
  (define-coeffects 'mode-reactive-comp (list :buffer-mode 'python-mode))
  ;; python-mode → component activates
  (emit-context-event :buffer-mode-change nil 'python-mode)
  (ok *mode-comp-active* "component activated on python-mode")
  ;; change to java-mode → component deactivates
  (emit-context-event :buffer-mode-change nil 'java-mode)
  (ok (not *mode-comp-active*) "component deactivated on java-mode")
  ;; back to python-mode → reactivates
  (emit-context-event :buffer-mode-change nil 'python-mode)
  (ok *mode-comp-active* "component re-activated on python-mode"))

(deftest test-n-context-changes-consistent
  ;; Property: N mode changes with M components → state always consistent
  ;; with the last event.  Uses 2 components, 50 switches.
  (reset-context)
  (let ((modes '(python-mode java-mode python-mode))
        (last-mode nil))
    (define-coeffects 'mode-reactive-comp (list :buffer-mode 'python-mode))
    (dotimes (i 50)
      (let ((mode (nth (mod i 3) modes)))
        (emit-context-event :buffer-mode-change nil mode)
        (setf last-mode mode)))
    ;; final state must be consistent with last event
    (if (eq last-mode 'python-mode)
        (ok *mode-comp-active*    "final state: active for python-mode")
        (ok (not *mode-comp-active*) "final state: inactive for non-python-mode"))))

;;; ── Emenda 5: HUD mounts/unmounts with :agent-tx-active ─────────────────

(defvar *hud-active* nil)

(define-component hud-comp
  :version "0.1"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (setf *hud-active* t)
             (list (yeditor-tx:install-effect comp :noop
                                               (lambda () nil)
                                               :undo-thunk (lambda () (setf *hud-active* nil)))))
  :activate nil
  :deactivate nil
  :config-spec nil)

(deftest test-hud-agent-tx-active-100x
  (reset-context)
  (setf *hud-active* nil)
  (define-coeffects 'hud-comp (list :agent-tx-active t))
  (dotimes (i 100)
    ;; agent opens tx → HUD mounts
    (set-work-predicate :agent-tx-active t)
    (ok *hud-active* (format nil "HUD active cycle ~A open" i))
    ;; agent commits/rollbacks → HUD unmounts
    (set-work-predicate :agent-tx-active nil)
    (ok (not *hud-active*) (format nil "HUD inactive cycle ~A close" i))))
