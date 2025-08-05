(defpackage :lem-core/commands/other
  (:use :cl :lem-core)
  (:export :nop-command
           :undefined-key
           :keyboard-quit
           :escape
           :exit-lem
           :quick-exit
           :execute-command
           :show-context-menu
           :load-library)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/other)

(defparameter *persist-commands* t
  "If non true, don't persist the history of commands called with M-x into Lem's home config directory.")

(define-key *global-keymap* "NopKey" 'nop-command)
(define-key *global-keymap* "C-g" 'keyboard-quit)
(define-key *global-keymap* "Escape" 'escape)
(define-key *global-keymap* "C-x C-c" 'exit-lem)
(define-key *global-keymap* "M-x" 'execute-command)
(define-key *global-keymap* "Shift-F10" 'show-context-menu)
(define-key *global-keymap* "M-h" 'show-context-menu)

(define-command nop-command () ()
  "No operation; it does nothing and return nil.")

(define-command undefined-key () ()
  "Signal undefined key error."
  (error 'undefined-key-error))

(define-command keyboard-quit () ()
  "Signal a `quit` condition."
  (error 'editor-abort :message nil))

(define-command escape () ()
  "Signal a `quit` condition silently."
  (error 'editor-abort :message nil))

(define-command exit-lem (&optional (ask t)) ()
  "Ask for modified buffers before exiting lem."
  (let ((modified-buffers
          (mapcar #'buffer-name (modified-buffers))))
    (and (or
          (null ask)
          (not modified-buffers)
          (prompt-for-y-or-n-p
           (format nil
                   "Modified buffers exist:~%~{~a~%~}Leave anyway?"
                   modified-buffers)))
         (exit-editor))))

(define-command quick-exit () ()
  "Exit the lem job and kill it."
  (lem-core/commands/file:save-some-buffers t)
  (exit-editor))

(defvar *commands-history*) ;; unbound

#+(or)
(makunbound '*commands-history*)

(defun commands-history ()
  "Return or create the commands' history struct.
  The history file is saved on (lem-home)/history/commands"
  (unless (boundp '*commands-history*)
    (let* ((pathname (merge-pathnames "history/commands" (lem-home)))
           (history (lem/common/history:make-history :pathname pathname)))
      (setf *commands-history* history)))
  *commands-history*)

(defun remember-command (input)
  "Add this command (string) to the history file."
  (when *persist-commands*
    (let ((history (commands-history)))
      (unless (stringp input)
        ;; Save a string, not a command object.
        (setf input (symbol-name (command-name input))))
      ;; find-command wants downcase strings.
      (setf input (str:downcase input))
      (and (lem/common/history:add-history history input
                                           :move-to-top t
                                           :allow-duplicates nil
                                           )
           (lem/common/history:save-file history)))))

(defun saved-commands ()
  "Return persisted commands names as a list."
  (reverse
   (lem/common/history:history-data-list (commands-history))))


(define-command execute-command (arg) (:universal-nil)
  "Read a command name, then read the ARG and call the command."
  (let* ((candidates (saved-commands))
         (name (prompt-for-command 
                (if arg
                    (format nil "~D M-x " arg)
                    "M-x ")
                :candidates candidates))
         (command (find-command name)))
    (if command
        (progn
          (when *persist-commands*
            (remember-command command))
          (call-command command arg))
        (message "invalid command"))))

(define-command show-context-menu () ()
  (let ((context-menu (buffer-context-menu (current-buffer))))
    (when context-menu
      (lem-core::update-point-on-context-menu-open (current-point))
      (lem-if:display-context-menu (implementation) context-menu '(:gravity :cursor)))))

(define-command load-library (name)
    ((prompt-for-library "load library: " :history-symbol 'load-library))
  "Load the Lisp library named NAME."
  (message "Loading ~A." name)
  (cond ((ignore-errors (maybe-load-systems (format nil "lem-~A" name) :silent t))
         (message "Loaded ~A." name))
        (t (message "Can't find Library ~A." name))))

(setf lem-core:*abort-key* 'keyboard-quit)
