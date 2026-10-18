;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-process
  (:use :cl :yeditor-tx)
  (:export :install-process))
(in-package :yeditor-effects/e-process)

(defun install-process (comp command &key output-buffer)
  "Spawn an external process as a transaction effect (kind 10, Cap. 5.3).
   COMMAND is a list of strings (program + args) passed to uiop:launch-program.
   OUTPUT-BUFFER is an optional Lem buffer for capturing stdout.
   Undo terminates the process and closes associated buffers."
  (let* ((cell (list nil))
         (proc-of (lambda () (car cell))))
    (let ((handle (install-effect comp :process
                                   (lambda ()
                                     (setf (car cell)
                                           (uiop:launch-program
                                            command
                                            :output (if output-buffer :stream nil)
                                            :error-output nil)))
                                   :undo-thunk
                                   (lambda ()
                                     (when (car cell)
                                       (handler-case
                                           (uiop:terminate-process (car cell) :urgent t)
                                         (error () nil)))
                                     (when output-buffer
                                       (handler-case (lem:kill-buffer output-buffer)
                                         (error () nil)))))))
      (push (cons :command command) handle)
      (push (cons :process-of proc-of) handle)
      handle)))
