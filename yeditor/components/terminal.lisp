;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/terminal
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :lem-terminal/terminal-mode
                #:make-terminal-buffer
                #:buffer-terminal
                #:resize-terminal)
  (:export :le-terminal
           :yeditor-terminal-toggle
           :*terminal-height*
           :*terminal-buffer-name*))
(in-package :yeditor-components/terminal)

(defparameter *terminal-height* 12
  "Height in lines of the integrated terminal panel.")

(defparameter *terminal-buffer-name* "*yeditor-terminal*"
  "Name of the integrated terminal buffer.")

(defun terminal-project-directory ()
  "Directory in which the integrated terminal should start."
  (or (ignore-errors (lem-core/commands/project:find-root (buffer-directory)))
      (buffer-directory)
      (uiop:getcwd)))

(defun ensure-terminal-buffer ()
  "Create or reuse the integrated terminal buffer."
  (or (get-buffer *terminal-buffer-name*)
      (let* ((dir (terminal-project-directory))
             (buffer (make-terminal-buffer (namestring dir))))
        (setf (buffer-name buffer) *terminal-buffer-name*)
        buffer)))

(defun terminal-active-p ()
  (and (frame-bottomside-window (current-frame))
       (eq (window-buffer (frame-bottomside-window (current-frame)))
           (get-buffer *terminal-buffer-name*))))

(define-command yeditor-terminal-toggle () ()
  "Toggle the integrated terminal panel (C-c j)."
  (if (terminal-active-p)
      (progn
        (ignore-errors (delete-bottomside-window))
        (setf (frame-bottomside-window (current-frame)) nil)
        (message "Terminal hidden"))
      (let* ((buffer (ensure-terminal-buffer))
             (window (make-bottomside-window buffer :height *terminal-height*)))
        (resize-terminal (buffer-terminal buffer) window)
        (message "Terminal shown"))))

(define-component le-terminal
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c j" (quote yeditor-terminal-toggle)))))
