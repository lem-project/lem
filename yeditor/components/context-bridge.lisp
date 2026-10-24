;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/context-bridge
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-context/reactivity #:update-contexts #:emit-context-event #:context-value)
  (:export :le-context-bridge
           :buffer-context
           :publish-buffer-context
           :*language-root-files*))
(in-package :yeditor-components/context-bridge)

;;;; Feeds the reactivity engine (T7) from the running editor: every file opened and
;;;; every buffer switched to publishes its file type, major mode and project root, so
;;;; components declared with coeffects (e.g. le-lang-python) activate on their own.

(defparameter *language-root-files*
  (list "pyproject.toml" "setup.py" "requirements.txt" "uv.lock" "package.json" "tsconfig.json"
        "Cargo.toml" "go.mod" "deps.edn")
  "Project markers added to the editor's root detection while the bridge is installed.")

(defun buffer-context (buffer)
  "Observable selected buffer, file, major mode, project root and window width."
  (let* ((file (buffer-filename buffer))
         (directory (if file (uiop:pathname-directory-pathname file) (buffer-directory buffer)))
         (root (and directory
                    (or (ignore-errors (lem-core/commands/project:find-root directory))
                        directory))))
    (list :buffer buffer :file file
          :window-width (window-width (current-window))
          :file-type (and file (pathname-type file))
          :buffer-mode (buffer-major-mode buffer)
          :project-root (and root (uiop:ensure-directory-pathname root)))))

(defun publish-buffer-context (buffer)
  (apply (function update-contexts) (buffer-context buffer)))

(defun publish-mode-change (buffer mode)
  (declare (ignore mode))
  (when (eq buffer (context-value :buffer)) (publish-buffer-context buffer)))

(defun publish-current-context ()
  ;; Most commands do not change context: avoid project filesystem probes per key.
  (let ((buffer (current-buffer)) (width (window-width (current-window))))
    (cond ((or (not (eq buffer (context-value :buffer)))
               (not (eq (buffer-major-mode buffer) (context-value :buffer-mode)))
               (not (equal (buffer-filename buffer) (context-value :file))))
           (publish-buffer-context buffer))
          ((not (eql width (context-value :window-width)))
           (emit-context-event :window-resize width)))))

(defun publish-buffer-close (buffer)
  (emit-context-event :buffer-close buffer))

(defun install-close-hook (comp)
  (let ((owned nil))
    (yeditor-tx:install-effect
     comp :hook
     (lambda ()
       (setf owned nil)
       (unless (find 'publish-buffer-close (variable-value 'kill-buffer-hook :global) :key #'car)
         (add-hook (variable-value 'kill-buffer-hook :global) 'publish-buffer-close)
         (setf owned (find 'publish-buffer-close (variable-value 'kill-buffer-hook :global)
                           :key #'car))))
     :undo-thunk
     (lambda ()
       (when (and owned (member owned (variable-value 'kill-buffer-hook :global) :test #'eq))
         (remove-hook (variable-value 'kill-buffer-hook :global) 'publish-buffer-close)
         (setf owned nil))))))

(define-component le-context-bridge
  :version "0.2.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp 'lem-core/commands/project:*root-files*
                                     (append lem-core/commands/project:*root-files* *language-root-files*))
                   (install-hook comp '*find-file-hook* 'publish-buffer-context)
                   (install-hook comp '*switch-to-buffer-hook* 'publish-buffer-context)
                   (install-hook comp '*buffer-mode-change-hook* 'publish-mode-change)
                   ;; Covers mode commands invoked directly, plus responsive window changes.
                   (install-hook comp '*post-command-hook* 'publish-current-context)
                   (install-close-hook comp))))
