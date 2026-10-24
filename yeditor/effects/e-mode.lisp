;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-mode
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-mode))
(in-package :yeditor-effects/e-mode)

(defun install-mode (comp buffer-or-name mode)
  "Queue MODE on BUFFER-OR-NAME, resolving the buffer at each activation.
Major modes restore the previous major mode; minor modes restore their previous
activation. A legacy function MODE takes the buffer and changes its major mode."
  (let ((buf nil) (prev-mode nil) (minor-p nil) (was-active-p nil))
    (let ((handle
            (install-effect
             comp :mode
             (lambda ()
               (setf buf (lem:get-buffer buffer-or-name))
               (unless (and buf (not (lem:deleted-buffer-p buf)))
                 (lem:editor-error "Buffer does not exist: ~A" buffer-or-name))
               (setf prev-mode (lem:buffer-major-mode buf)
                     minor-p (and (symbolp mode) (member mode (lem:minor-modes)))
                     was-active-p (and minor-p (lem:mode-active-p buf mode)))
               (cond (minor-p
                      (unless was-active-p (lem:change-buffer-mode buf mode t)))
                     ((symbolp mode) (lem:change-buffer-mode buf mode))
                     (t (funcall mode buf))))
             :undo-thunk
             (lambda ()
               (when (and buf (not (lem:deleted-buffer-p buf)))
                 (if minor-p
                     (unless was-active-p (lem:change-buffer-mode buf mode nil))
                     (when prev-mode (lem:change-buffer-mode buf prev-mode))))))))
      (push (cons :mode mode) handle)
      (push (cons :buffer buffer-or-name) handle)
      handle)))
