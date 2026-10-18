;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;;; le-director-cockpit (plan T12-B, Director Mode): the intent bar. The user
;;;; types or dictates a single natural-language line and the agent — through the
;;;; le-mcp editor tools — operates the editor: it opens files, splits windows,
;;;; edits buffers under an open agent transaction and reports back.
;;;;
;;;; One key (F1) replaces every M-x chord. Approve/revert reuse the HUD's agent
;;;; transaction lifecycle (C-c C-a / C-c C-k) under stable names
;;;; (yeditor-director-approve / yeditor-director-rollback) so the future Webview
;;;; bridge can click buttons mapped 1:1 to these commands:
;;;;   ye:director-intent   -> yeditor-director-intent
;;;;   ye:tx-commit-active  -> yeditor-director-approve
;;;;   ye:tx-rollback-active-> yeditor-director-rollback

(defpackage :yeditor-components/director-cockpit
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/mcp
                #:worker-ids #:worker-name #:submit-task)
  (:export :le-director-cockpit
           :yeditor-director-intent
           :yeditor-director-approve
           :yeditor-director-rollback
           :*director-worker-name*))
(in-package :yeditor-components/director-cockpit)

(defparameter *director-worker-name* "hermes"
  "Worker that receives director intents (matched by name prefix, e.g. 'hermes').")

(defun director-worker-id ()
  "The first registered worker whose name contains *director-worker-name*."
  (loop :for id :in (worker-ids)
        :when (ignore-errors
               (search *director-worker-name* (worker-name id)
                       :test (function string-equal)))
        :return id))

(define-command yeditor-director-intent (prompt) ("s🎯 Intent for the agent: ")
  "Submit one natural-language intent to the agent bus and open an agent
transaction: the HUD mounts and the agent operates the editor through the
le-mcp editor tools."
  (let ((worker (or (director-worker-id) "unregistered")))
    (submit-task worker "intent" prompt)
    (message "Intent sent to ~A; the HUD shows the pending edits (C-c C-a approves, C-c C-k reverts)."
             worker)))

(define-command yeditor-director-approve () ()
  "Approve and consolidate the agent's work (same as C-c C-a)."
  (yeditor-components/hud:yeditor-agent-tx-commit))

(define-command yeditor-director-rollback () ()
  "Revert everything the agent did (same as C-c C-k)."
  (yeditor-components/hud:yeditor-agent-tx-rollback))

(define-component le-director-cockpit
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "F1" (quote yeditor-director-intent)))))