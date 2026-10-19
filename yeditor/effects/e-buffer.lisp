;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-buffer
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-buffer))
(in-package :yeditor-effects/e-buffer)

(defun install-buffer (comp name)
  "Queue a buffer acquisition for COMP; undo deletes only a buffer it created.
Recheck ownership on each activation, preserving buffers that already exist."
  (let ((buf nil) (owned-p nil))
    (let ((handle (install-effect
                   comp :buffer
                   (lambda ()
                     (setf owned-p (null (lem:get-buffer name))
                           buf (lem:make-buffer name :enable-undo-p nil)))
                   :undo-thunk
                   (lambda ()
                     (when (and owned-p buf (not (lem:deleted-buffer-p buf)))
                       ;; KILL-BUFFER deliberately keeps the last user buffer;
                       ;; transaction cleanup must also release that resource.
                       (lem:delete-buffer buf))
                     (setf buf nil owned-p nil)))))
      (push (cons :name name) handle)
      handle)))
