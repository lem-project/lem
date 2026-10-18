(defpackage :yeditor-effects/e-process
  (:use :cl :yeditor-tx)
  (:export :install-process))
(in-package :yeditor-effects/e-process)

(defun install-process (comp command &key output-buffer)
  "Spawn an external process as a transaction effect (kind 10, Cap. 5.3).
   COMMAND is a list of strings (program + args) passed to uiop:launch-program.
   OUTPUT-BUFFER is an optional Lem buffer for capturing stdout.
   Undo terminates the process and closes associated buffers."
  (let ((proc nil))
    (let ((handle (install-effect comp :process
                                   (lambda ()
                                     (setf proc
                                           (uiop:launch-program
                                            command
                                            :output (if output-buffer :stream :output)
                                            :error-output :output)))
                                   :undo-thunk
                                   (lambda ()
                                     (when proc
                                       (handler-case
                                           (uiop:terminate-process proc :urgent t)
                                         (error () nil)))
                                     (when output-buffer
                                       (handler-case (lem:kill-buffer output-buffer)
                                         (error () nil)))))))
      (push (cons :command command) handle)
      handle)))
