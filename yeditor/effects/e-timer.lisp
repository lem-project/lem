;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-timer
  (:use :cl :lem :yeditor-tx)
  (:export :install-timer))
(in-package :yeditor-effects/e-timer)

(defun install-timer (comp ms fn &key repeat)
  "Schedule a Lem timer that fires FN every MS milliseconds as a transaction
   effect (kind 9, Cap. 5.3).  If REPEAT is true the timer fires repeatedly;
   otherwise it fires once.  Undo stops the timer."
  (let ((timer nil))
    (let ((handle (install-effect comp :timer
                                   (lambda ()
                                     (setf timer (lem:make-timer fn))
                                     (lem:start-timer timer ms :repeat repeat))
                                   :undo-thunk
                                   (lambda ()
                                     (when timer (lem:stop-timer timer))))))
      (push (cons :ms ms) handle)
      (push (cons :repeat repeat) handle)
      handle)))
