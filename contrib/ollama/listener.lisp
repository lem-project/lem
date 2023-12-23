(defpackage :lem-ollama/listener
  (:use :cl :lem :lem-ollama))
(in-package :lem-ollama/listener)

(define-major-mode ollama-mode nil 
    (:name "ollama"
     :keymap *ollama-mode-keymap*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(define-key *ollama-mode-keymap* "C-c C-c" 'ollama-cancel)

(defun reset-listener-variables (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        #'execute-input))

(defun execute-input (point string)
  (bt2:make-thread
   (lambda ()
     (with-open-stream (out (make-buffer-output-stream point))
       (ollama-request string)
       (handle-stream out)))))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*ollama*")))
    (unless (eq (buffer-major-mode buffer) 'ollama-mode)
      (change-buffer-mode buffer 'ollama-mode))
    buffer))

(defun run-ollama-internal ()
  (pop-to-buffer (get-repl-buffer)))

(define-command run-ollama () () 
  (run-ollama-internal))