(defpackage :lem-core/commands/other
  (:use :cl :lem-core)
  (:export :undefined-key
           :keyboard-quit
           :escape
           :exit-lem
           :quick-exit
           :execute-command
           :show-context-menu
           :load-library))
(in-package :lem-core/commands/other)

(define-key *global-keymap* "NopKey" 'nop-command)
(define-key *global-keymap* "C-g" 'keyboard-quit)
(define-key *global-keymap* "Escape" 'escape)
(define-key *global-keymap* "C-x C-c" 'exit-lem)
(define-key *global-keymap* "M-x" 'execute-command)
(define-key *global-keymap* "Shift-F10" 'show-context-menu)

(define-command nop-command () ())

(define-command undefined-key () ()
  (editor-error "Key not found: ~A"
                (keyseq-to-string (last-read-key-sequence))))

(define-command keyboard-quit () ()
  (error 'editor-abort))

(define-command escape () ()
  (error 'editor-abort :message nil))

(define-command exit-lem (&optional (ask t)) ()
  (when (or (null ask)
            (not (any-modified-buffer-p))
            (prompt-for-y-or-n-p "Modified buffers exist. Leave anyway"))
    (exit-editor)))

(define-command quick-exit () ()
  (uiop:symbol-call :lem :save-some-buffers t) ; TODO: resolve save-some-buffers dependencies
  (exit-editor))

(define-command execute-command (arg) ("P")
  (let* ((name (prompt-for-string
                (if arg
                    (format nil "~D M-x " arg)
                    "M-x ")
                :completion-function (lambda (str)
                                       (if (find #\- str)
                                           (completion-hypheen str (all-command-names))
                                           (completion str (all-command-names))))
                :test-function 'exist-command-p
                :history-symbol 'mh-execute-command))
         (command (find-command name)))
    (if command
        (call-command command arg)
        (message "invalid command"))))

(define-command show-context-menu () ()
  (let ((context-menu (buffer-context-menu (current-buffer))))
    (when context-menu
      (lem-if:display-context-menu (implementation) context-menu '(:gravity :cursor)))))

(define-command load-library (name)
    ((lem-core::prompt-for-library "load library: " :history-symbol 'load-library))
  (message "Loading ~A." name)
  (cond ((ignore-errors (maybe-quickload (format nil "lem-~A" name) :silent t))
         (message "Loaded ~A." name))
        (t (message "Can't find Library ~A." name))))

(setf lem-core:*abort-key* 'keyboard-quit)
