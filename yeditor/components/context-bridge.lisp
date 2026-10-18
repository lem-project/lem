;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/context-bridge
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-context/reactivity #:update-contexts)
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
  "Plist (:file-type :buffer-mode :project-root) describing BUFFER."
  (let* ((file (buffer-filename buffer))
         (directory (if file (uiop:pathname-directory-pathname file) (buffer-directory buffer)))
         (root (and directory
                    (or (ignore-errors (lem-core/commands/project:find-root directory))
                        directory))))
    (list :file-type (and file (pathname-type file))
          :buffer-mode (buffer-major-mode buffer)
          :project-root (and root (uiop:ensure-directory-pathname root)))))

(defun publish-buffer-context (buffer)
  (apply (function update-contexts) (buffer-context buffer)))

(define-component le-context-bridge
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp (quote lem-core/commands/project:*root-files*)
                                     (append lem-core/commands/project:*root-files* *language-root-files*))
                   (install-hook comp (quote *find-file-hook*) (quote publish-buffer-context))
                   (install-hook comp (quote *switch-to-buffer-hook*) (quote publish-buffer-context)))))
