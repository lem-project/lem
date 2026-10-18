;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/sidebar
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-sidebar
           :yeditor-sidebar-toggle
           :yeditor-sidebar-focus
           :yeditor-sidebar-refresh
           :sidebar-active-p
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
    (setf (buffer-value buffer 'sidebar-keymap) t)
    (let ((root (lem/filer::create-directory-item directory :open t)))
      (lem/filer:render buffer root)
      (setf (lem/filer:root-item buffer) root))
    buffer))

(defun sidebar-focus-editor ()
  "Move focus from the sidebar to the main editor window."
  (let ((left (frame-leftside-window (current-frame))))
    (loop :for w := (next-window)
          :repeat 20
          :until (and (not (eq w left)) (not (typep w 'lem-core:side-window)))
          :finally (switch-to-window w))))

(defun focus-sidebar-window ()
  (alexandria:when-let ((left (frame-leftside-window (current-frame))))
    (switch-to-window left)))

(define-command yeditor-sidebar-focus-editor () ()
  "Leave the sidebar focused window and return to the editor."
  (sidebar-focus-editor))

(define-command yeditor-sidebar-refresh () ()
  "Reload the sidebar tree from disk."
  (let ((buffer (get-buffer *sidebar-buffer-name*)))
    (when buffer
      (let ((root (lem/filer:root-item buffer)))
        (when (typep root 'lem/filer::directory-item)
          (setf (lem/filer::directory-item-open-p root) t)
          (setf (lem/filer::directory-item-children root)
                (lem/filer::create-directory-children (lem/filer::item-pathname root)))
          (lem/filer:render buffer root))
        (message "Sidebar refreshed")))))

(define-command yeditor-sidebar-focus () ()
  "Focus the sidebar window without toggling it (C-c C-b)."
  (focus-sidebar-window))

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
             (list
              (install-keybinding comp *global-keymap* "C-c b" (quote yeditor-sidebar-toggle))
              (install-keybinding comp *global-keymap* "C-c C-b" (quote yeditor-sidebar-focus))
              ;; quick keys inside the sidebar buffer (filer mode keymap)
              (install-keybinding comp lem/filer:*filer-mode-keymap* "q"
                                  (quote yeditor-sidebar-focus-editor))
              (install-keybinding comp lem/filer:*filer-mode-keymap* "r"
                                  (quote yeditor-sidebar-refresh)))))
