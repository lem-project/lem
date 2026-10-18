;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-overlay
  (:use :cl :lem :yeditor-tx)
  (:export :install-overlay))
(in-package :yeditor-effects/e-overlay)

(defun install-overlay (comp start end attribute)
  "Create a Lem overlay from START to END with ATTRIBUTE as a transaction effect
   (kind 8, Cap. 5.3).  START and END are lem:point objects; ATTRIBUTE is a
   lem:attribute.  Undo deletes the overlay."
  (let ((ov nil))
    (let ((handle (install-effect comp :overlay/face
                                   (lambda ()
                                     (setf ov (lem:make-overlay start end attribute)))
                                   :undo-thunk
                                   (lambda ()
                                     (when ov (lem:delete-overlay ov))))))
      (push (cons :start start) handle)
      (push (cons :end end) handle)
      handle)))
