;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/palette
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-palette
           :*palette-themes-function*
           :*palette-theme-aliases*
           :*palette-actions*
           :yeditor-palette-theme
           :call-with-theme-preview
           :*open-preview-sessions*))
(in-package :yeditor-components/palette)

(defvar *palette-themes-function* (function color-theme-names)
  "Function of no arguments returning the theme names the palette offers.
   le-themes-ergonomic narrows it to the curated pack; le-theme-extended widens it.")

(defvar *palette-theme-aliases* nil
  "Alist mapping human palette labels to registered Lem theme names.")

(defvar *palette-actions* nil
  "Alist of palette labels and zero-argument actions, committed only on acceptance.")

(defvar *open-preview-sessions* 0
  "Number of live-preview sessions currently open; 0 whenever no palette is shown.")

(defun resolve-theme-name (name)
  (or (cdr (assoc name *palette-theme-aliases* :test #'equal)) name))

(defun palette-theme-p (name)
  (or (assoc name *palette-actions* :test #'equal)
      (find-color-theme (resolve-theme-name name))))

(defun call-with-theme-preview (function)
  "Run FUNCTION with a preview closure (NAME) that applies a theme without saving it.
   FUNCTION returns a theme name or an action label. Themes are applied and
   saved; actions execute only on acceptance, after restoring the initial theme.
   On any non-local exit (C-g signals editor-abort) or a nil result, the theme
   active before the session is restored."
  (let ((original (current-theme))
        (chosen nil))
    (incf *open-preview-sessions*)
    (unwind-protect
         (setf chosen (funcall function
                               (lambda (name)
                                 (let ((theme (resolve-theme-name name)))
                                   (when (find-color-theme theme)
                                     (load-theme theme nil))))))
      (decf *open-preview-sessions*)
      (cond ((assoc chosen *palette-actions* :test #'equal)
             (when (and original (not (equal original (current-theme))))
               (load-theme original nil))
             (funcall (cdr (assoc chosen *palette-actions* :test #'equal))))
            (chosen (load-theme (resolve-theme-name chosen) t))
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
          (completion string (append (mapcar #'car *palette-actions*)
                                     (funcall *palette-themes-function*)))))

(define-command yeditor-palette-theme () ()
  "Choose an appearance action or preview a theme; cancel restores the previous theme."
  (call-with-theme-preview
   (lambda (preview)
     (prompt-for-string "Appearance (live preview, C-g to cancel): "
                        :completion-function (lambda (string) (theme-items string preview))
                        :test-function (quote palette-theme-p)))))

(define-component le-palette
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-keybinding comp *global-keymap* "C-c p" (quote yeditor-palette-theme)))))
