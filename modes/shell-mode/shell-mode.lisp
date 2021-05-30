(defpackage :lem-shell-mode
  (:use :cl :lem :alexandria)
  (:export)
  #+sbcl
  (:lock t))
(in-package :lem-shell-mode)

(defparameter *command* '("/usr/local/bin/bash"))

(defun buffer-process (buffer)
  (buffer-value buffer 'process))

(defun process-buffer (process)
  (find process (buffer-list) :key #'buffer-process))

(define-major-mode run-shell-mode nil
    (:name "Shell"
     :keymap *run-shell-mode-keymap*)
  (reset-listener-variable (current-buffer))
  (lem.listener-mode:listener-mode t))

(defun reset-listener-variable (buffer)
  (setf (variable-value 'lem.listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem.listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem.listener-mode:listener-execute-function :buffer buffer)
        'execute-input))

(defun execute-input (point string)
  (setf string (concatenate 'string string (string #\newline)))
  (lem-process:process-send-input (buffer-process (point-buffer point))
                                  string))

(defparameter *buffer-name* "*shell*")

(defun delete-shell-buffer (buffer)
  (lem-process:delete-process (buffer-process buffer))
  buffer)

(defun shell-buffer-name (process)
  (format nil "~A pid:~D"
          (lem-process::process-name process)
          (async-process::process-pid (lem-process::process-pointer process))))

(defun create-shell-buffer (process)
  (let ((buffer (make-buffer (shell-buffer-name process))))
    (unless (eq (buffer-major-mode buffer) 'run-shell-mode)
      (change-buffer-mode buffer 'run-shell-mode))
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
              'delete-shell-buffer)
    (setf (buffer-value buffer 'process) process)
    buffer))

(defun output-callback (process string)
  (when-let* ((buffer (process-buffer process))
              (point (buffer-point buffer)))
    (buffer-end point)
    (lem-lisp-mode::insert-escape-sequence-string point string)
    ;; (insert-string point string)
    (lem.listener-mode:listener-reset-prompt buffer nil)))

(defun run-shell-internal ()
  (create-shell-buffer
   (lem-process:run-process *command*
                            :name "shell"
                            :output-callback 'output-callback
                            :output-callback-type :process-input)))

(define-command run-shell () ()
  (setf (current-window)
        (display-buffer (run-shell-internal))))

