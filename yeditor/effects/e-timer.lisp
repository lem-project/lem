;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-timer
  (:use :cl :lem :yeditor-tx)
  (:export :install-timer))
(in-package :yeditor-effects/e-timer)

(defun install-timer (comp ms fn &key repeat)
  "Schedule a Lem timer that fires FN every MS milliseconds as a transaction
   effect (kind 9, Cap. 5.3).  If REPEAT is true the timer fires repeatedly;
   otherwise it fires once.  Undo stops the timer and invalidates queued callbacks from that activation."
  (let ((timer nil)
        (activation nil))
    (let ((handle (install-effect comp :timer
                                   (lambda ()
                                     (let ((token (list t)))
                                       (setf activation token
                                             timer (lem:make-timer
                                                    (lambda ()
                                                      (when (car token) (funcall fn))))))
                                     (lem:start-timer timer ms :repeat repeat))
                                   :undo-thunk
                                   (lambda ()
                                     (when activation
                                       (setf (car activation) nil
                                             activation nil))
                                     (when timer
                                       (lem:stop-timer timer)
                                       (setf timer nil))))))
      (push (cons :ms ms) handle)
      (push (cons :repeat repeat) handle)
      handle)))
