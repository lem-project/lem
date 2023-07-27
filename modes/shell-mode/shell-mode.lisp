(defpackage :lem-shell-mode
  (:use :cl :lem :alexandria)
  (:export :*default-shell-command*)
  #+sbcl
  (:lock t))
(in-package :lem-shell-mode)

(defvar *default-shell-command* nil "Set if you do want to use non default shell. '(\"/usr/local/bin/bash\")")

(defun shell-command ()
  (or *default-shell-command*
      (let ((shell
              (or
               #-windows
               (uiop:getenv "SHELL")
               #+windows
               (let ((windir (uiop:getenv "windir")))
                 (and windir
                      (merge-pathnames "system32/cmd.exe" windir))))))
        (list shell))))

(defun buffer-process (buffer)
  (buffer-value buffer 'process))

(defun process-buffer (process)
  (find process (buffer-list) :key #'buffer-process))

(define-major-mode run-shell-mode nil
    (:name "Shell"
     :keymap *run-shell-mode-keymap*)
  (reset-listener-variable (current-buffer))
  (lem/listener-mode:start-listener-mode))

(defun reset-listener-variable (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        'execute-input
        (variable-value 'lem/listener-mode:listener-prompt-attribute :buffer buffer)
        nil))

(defun execute-input (point string)
  (setf string (concatenate 'string string (string #\newline)))
  (lem-process:process-send-input (buffer-process (point-buffer point))
                                  string))

(defun delete-shell-buffer (buffer)
  (lem-process:delete-process (buffer-process buffer))
  buffer)

(defun shell-buffer-name (process)
  (format nil "*~A pid:~D*"
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
    ;; TODO: lisp-modeに依存するのはおかしいので汎用的なパッケージを用意する
    (lem-lisp-mode/internal::insert-escape-sequence-string point string)
    ;; (insert-string point string)
    (lem/listener-mode:refresh-prompt buffer nil)))

(defun run-shell-internal ()
  (create-shell-buffer
   (lem-process:run-process (shell-command)
                            :name "shell"
                            :output-callback 'output-callback
                            :output-callback-type :process-input)))

(define-command run-shell () ()
  (switch-to-window
        (pop-to-buffer (run-shell-internal))))
