(defpackage #:lem-emacs-help-mode
  (:use #:cl #:lem))
(in-package #:lem-emacs-help-mode)

(defvar *emacs-help-mode-keymap* (make-keymap))
(defvar *ctrl-h-keymap* (make-keymap))
(defvar *previous-describe-output-override* nil)

(define-key *ctrl-h-keymap* "k" 'describe-key)
(define-key *ctrl-h-keymap* "b" 'describe-bindings)
(define-key *ctrl-h-keymap* "m" 'describe-mode)
(define-key *ctrl-h-keymap* "a" 'apropos-command)
(define-key *ctrl-h-keymap* "v" 'apropos-variable)
;; TODO add describe-function command for "f"

(define-key *emacs-help-mode-keymap* "C-h" *ctrl-h-keymap*)

(defun enable ()
  "Enables emacs help mode"

  ;; TODO, figure out a more functional way to overwrite this
  ;; variable which does not cause mutable global state
  (setf *previous-describe-output-override*
        lem-core/commands/help:*describe-output-type-override*)
  (setf lem-core/commands/help:*describe-output-type-override* :buffer))

(defun disable ()
  "Disables emacs help mode"
  (setf lem-core/commands/help:*describe-output-type-override*
        *previous-describe-output-override*)
  (setf *previous-describe-output-override* nil))

(define-minor-mode emacs-help-mode
    (:name "EHelp"
     :description "Adds Emacs Style C-h Bindings."
     :global t
     :keymap *emacs-help-mode-keymap*
     :enable-hook 'enable
     :disable-hook 'disable))
