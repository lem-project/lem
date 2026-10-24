;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/mode-line
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/palette #:*palette-actions*)
  (:export #:le-mode-line-minimal #:le-mode-line-buffer #:le-mode-line-mode
           #:le-mode-line-git #:le-mode-line-diagnostics #:le-mode-line-position #:le-tabs))
(in-package :yeditor-components/mode-line)

(defvar *buffer-enabled* nil)
(defvar *mode-enabled* nil)
(defvar *git-enabled* nil)
(defvar *diagnostics-enabled* nil)
(defvar *position-enabled* nil)

(defun buffer-segment (window)
  (if *buffer-enabled*
      (multiple-value-bind (name attribute) (modeline-name window)
        (values (concatenate 'string " " name " ") attribute))
      ""))

(defun mode-segment (window)
  (if *mode-enabled* (modeline-major-mode window) ""))

(defun position-segment (window)
  (if *position-enabled* (modeline-position window) ""))

(defun git-segment (window)
  ;; Lem exposes no public accessor for its cached gutter data. Read its buffer
  ;; key without invoking Git during redisplay (which would block the UI).
  (let ((changes (and *git-enabled*
                      (buffer-value (window-buffer window) 'lem-git-gutter::git-gutter-changes))))
    (if changes (format nil " Git:~D " (hash-table-count changes)) "")))

(defun diagnostics-segment (window)
  (if *diagnostics-enabled*
      (let ((diagnostics (lem-lsp-mode:buffer-diagnostic-list (window-buffer window))))
        (if diagnostics (format nil " Diagnostics:~D " (length diagnostics)) ""))
      ""))

(defun component-action (label component)
  (cons label (lambda () (toggle-component component))))

(define-component le-mode-line-minimal
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-editor-variable
                    comp 'modeline-format
                    '(buffer-segment mode-segment git-segment diagnostics-segment
                      (position-segment nil :right)))
                   (install-variable
                    comp '*palette-actions*
                    (append
                     (mapcar (lambda (entry) (component-action (car entry) (cdr entry)))
                             '(("Mode-line: buffer" . le-mode-line-buffer)
                               ("Mode-line: mode" . le-mode-line-mode)
                               ("Mode-line: Git changes" . le-mode-line-git)
                               ("Mode-line: diagnostics" . le-mode-line-diagnostics)
                               ("Mode-line: position" . le-mode-line-position)
                               ("Tabs: toggle" . le-tabs)))
                     *palette-actions*)))))

(define-component le-mode-line-buffer
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*buffer-enabled* t))))

(define-component le-mode-line-mode
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*mode-enabled* t))))

(define-component le-mode-line-git
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*git-enabled* t))))

(define-component le-mode-line-diagnostics
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*diagnostics-enabled* t))))

(define-component le-mode-line-position
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp '*position-enabled* t))))

(define-component le-tabs
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-editor-variable comp 'lem/frame-multiplexer:frame-multiplexer t))))
