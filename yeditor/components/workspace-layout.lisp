;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/workspace-layout
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/sidebar
                #:ensure-sidebar-buffer
                #:*sidebar-buffer-name*)
  (:export :le-workspace-layout
           :yeditor-workspace-open
           :yeditor-workspace-close))
(in-package :yeditor-components/workspace-layout)

(defun workspace-active-p ()
  "True when at least the sidebar is open (proxy for workspace mode)."
  (and (frame-leftside-window (current-frame))
       (eq (window-buffer (frame-leftside-window (current-frame)))
           (get-buffer *sidebar-buffer-name*))))

(define-command yeditor-workspace-open () ()
  "Open the modern IDE workspace layout: sidebar + editor."
  (unless (workspace-active-p)
    (alexandria:when-let ((buffer (ensure-sidebar-buffer)))
      (make-leftside-window buffer :width yeditor-components/sidebar::*sidebar-width*)
      (message "Workspace layout opened"))))

(define-command yeditor-workspace-close () ()
  "Close the workspace layout (sidebar)."
  (when (workspace-active-p)
    (delete-leftside-window))
  (message "Workspace layout closed"))

(defun yeditor-workspace-open-hook (buffer)
  "Hook wrapper: open workspace layout when BUFFER is a real file."
  (declare (ignore buffer))
  (yeditor-workspace-open))

(define-component le-workspace-layout
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c w" (quote yeditor-workspace-open))
                   (install-hook comp '*find-file-hook* (quote yeditor-workspace-open-hook)))))
