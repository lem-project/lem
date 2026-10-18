;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/palette
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-palette
           :*palette-themes-function*
           :yeditor-palette-theme
           :call-with-theme-preview
           :*open-preview-sessions*))
(in-package :yeditor-components/palette)

(defvar *palette-themes-function* (function color-theme-names)
  "Function of no arguments returning the theme names the palette offers.
   le-themes-ergonomic narrows it to the curated pack; le-theme-extended widens it.")

(defvar *open-preview-sessions* 0
  "Number of live-preview sessions currently open; 0 whenever no palette is shown.")

(defun call-with-theme-preview (function)
  "Run FUNCTION with a preview closure (NAME) that applies a theme without saving it.
   FUNCTION returns the chosen theme name. If it returns a name, that theme is
   applied and saved; on any non-local exit (C-g signals editor-abort) or a nil
   result, the theme active before the session is restored."
  (let ((original (current-theme))
        (chosen nil))
    (incf *open-preview-sessions*)
    (unwind-protect
         (setf chosen (funcall function
                               (lambda (name)
                                 (when (find-color-theme name)
                                   (load-theme name nil)))))
      (decf *open-preview-sessions*)
      (cond (chosen (load-theme chosen t))
            ((and original (not (equal original (current-theme))))
             (load-theme original nil))))
    chosen))

(defun theme-items (string preview)
  (mapcar (lambda (name)
            (lem/completion-mode:make-completion-item
             :label name
             :focus-action (lambda (context)
                             (declare (ignore context))
                             (funcall preview name))))
          (completion string (funcall *palette-themes-function*))))

(define-command yeditor-palette-theme () ()
  "Pick a color theme with live preview; C-g (abort) restores the previous theme."
  (call-with-theme-preview
   (lambda (preview)
     (prompt-for-string "Theme (live preview, C-g to cancel): "
                        :completion-function (lambda (string) (theme-items string preview))
                        :test-function (quote find-color-theme)))))

(define-component le-palette
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c p" (quote yeditor-palette-theme)))))
