(defpackage #:lem-emacs-help-mode
  (:use #:cl #:lem))
(in-package #:lem-emacs-help-mode)

(defvar *emacs-help-mode-keymap* (make-keymap))
(defvar *ctrl-h-keymap* (make-keymap))

(define-key *ctrl-h-keymap* "k" 'describe-key)
(define-key *ctrl-h-keymap* "b" 'describe-bindings)
(define-key *ctrl-h-keymap* "m" 'describe-mode)
(define-key *ctrl-h-keymap* "a" 'apropos-command)
(define-key *ctrl-h-keymap* "v" 'describe-lem-variable)
;; TODO add describe-function command for "f"

(define-key *emacs-help-mode-keymap* "C-h" *ctrl-h-keymap*)

(defun enable ()
  "Enables emacs help mode"
  (when lem-core/commands/help:*documentation-output-style*
    (lem:editor-error "Warning: Overwriting *documentation-output-style*"))

  (setf lem-core/commands/help:*documentation-output-style* :buffer))

(defun disable ()
  "Disables emacs help mode"
  (setf lem-core/commands/help:*documentation-output-style* nil))

(define-minor-mode emacs-help-mode
    (:name "EHelp"
     :description "Adds Emacs Style C-h Bindings."
     :global t
     :keymap *emacs-help-mode-keymap*
     :enable-hook 'enable
     :disable-hook 'disable))
