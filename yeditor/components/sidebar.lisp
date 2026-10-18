;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/sidebar
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-sidebar
           :yeditor-sidebar-toggle
           :*sidebar-width*))
(in-package :yeditor-components/sidebar)

(defparameter *sidebar-width* 32
  "Width in columns of the persistent project sidebar.")

(defparameter *sidebar-buffer-name* "*yeditor-sidebar*"
  "Name of the sidebar filer buffer.")

(defun sidebar-project-directory ()
  "Return the directory the sidebar should root itself in."
  (or (ignore-errors (lem-core/commands/project:find-root (buffer-directory)))
      (buffer-directory)
      (uiop:getcwd)))

(defun make-sidebar-buffer (directory)
  "Create a filer buffer rooted at DIRECTORY for the persistent sidebar."
  (let* ((directory (probe-file directory))
         (buffer (make-buffer *sidebar-buffer-name* :temporary t)))
    (change-buffer-mode buffer 'lem/filer:filer-mode)
    (let ((root (lem/filer::create-directory-item directory :open t)))
      (lem/filer:render buffer root)
      (setf (lem/filer:root-item buffer) root))
    buffer))

(defun ensure-sidebar-buffer ()
  "Create or reuse the sidebar filer buffer for the current project."
  (let ((dir (sidebar-project-directory))
        (buffer (get-buffer *sidebar-buffer-name*)))
    (cond ((null buffer)
           (make-sidebar-buffer dir))
          ((and (lem/filer:root-item buffer)
                (uiop:pathname-equal (lem/filer::item-pathname (lem/filer:root-item buffer)) dir))
           buffer)
          (t
           (make-sidebar-buffer dir)))))

(defun sidebar-active-p ()
  (and (frame-leftside-window (current-frame))
       (eq (window-buffer (frame-leftside-window (current-frame)))
           (get-buffer *sidebar-buffer-name*))))

(define-command yeditor-sidebar-toggle () ()
  "Toggle the persistent project sidebar (C-c b)."
  (if (sidebar-active-p)
      (progn
        (ignore-errors (delete-leftside-window))
        (setf (frame-leftside-window (current-frame)) nil)
        (message "Sidebar hidden"))
      (let ((buffer (ensure-sidebar-buffer)))
        (make-leftside-window buffer :width *sidebar-width*)
        (message "Sidebar shown"))))

(define-component le-sidebar
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c b" (quote yeditor-sidebar-toggle)))))
