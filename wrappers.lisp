;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(shell-command))

(defun shell-command (command &key input output error-output)
  (uiop:run-program command
                    :input input
                    :output output
                    :error-output error-output
                    :ignore-error-status t))
