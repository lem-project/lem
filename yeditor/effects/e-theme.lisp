;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-theme
  (:use :cl :lem :yeditor-tx)
  (:export :install-theme))
(in-package :yeditor-effects/e-theme)

(defun install-theme (comp name)
  "Apply color theme NAME without saving it to the user config, as a transaction effect
   (kind 8, faces). The theme active at commit time is restored on undo."
  (let ((prev nil))
    (let ((handle (install-effect comp :theme
                                  (lambda ()
                                    (setf prev (current-theme))
                                    (load-theme name nil))
                                  :undo-thunk
                                  (lambda ()
                                    (when prev (load-theme prev nil))))))
      (push (cons :theme name) handle)
      handle)))
