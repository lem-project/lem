(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem.universal-argument
        :lem-vi-mode.commands))
(in-package :lem-vi-mode)

(defvar *command-keymap* (make-keymap :name '*command-keymap* :insertion-hook 'undefined-key))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap*))

(defvar *modeline-element*)

(define-attribute state-attribute
  (t :reverse-p t))

(defstruct vi-state
  name
  keymap
  function)

(defstruct (vi-modeline-element (:conc-name element-))
  name)

(defmethod convert-modeline-element ((element vi-modeline-element) window)
  (values (element-name element) 'state-attribute))

(defmacro define-vi-state (name (&key keymap) &body body)
  `(setf (get ',name 'state)
         (make-vi-state :name ',name :keymap ,keymap :function (lambda () ,@body))))

(defun trans-state (name)
  (let ((state (get name 'state)))
    (assert (vi-state-p state))
    (setf (mode-keymap 'vi-mode) (vi-state-keymap state))
    (funcall (vi-state-function state))))

(define-vi-state command (:keymap *command-keymap*)
  (setf (element-name *modeline-element*) "[NORMAL]"))

(define-vi-state insert (:keymap *insert-keymap*)
  (message " -- INSERT --")
  (setf (element-name *modeline-element*) "[INSERT]"))

(defun enable-hook ()
  (setf *modeline-element* (make-vi-modeline-element))
  (modeline-add-status-list *modeline-element*)
  (trans-state 'command))

(defun disable-hook ()
  (modeline-remove-status-list *modeline-element*))

(define-minor-mode vi-mode
    (:global t
     :keymap *command-keymap*
     :enable-hook #'enable-hook
     :disable-hook #'disable-hook))

(define-key *command-keymap* "0" 'vi-move-to-beginning-of-line/universal-argument-0)
(define-key *command-keymap* "1" 'universal-argument-1)
(define-key *command-keymap* "2" 'universal-argument-2)
(define-key *command-keymap* "3" 'universal-argument-3)
(define-key *command-keymap* "4" 'universal-argument-4)
(define-key *command-keymap* "5" 'universal-argument-5)
(define-key *command-keymap* "6" 'universal-argument-6)
(define-key *command-keymap* "7" 'universal-argument-7)
(define-key *command-keymap* "8" 'universal-argument-8)
(define-key *command-keymap* "9" 'universal-argument-9)
(define-key *command-keymap* "l" 'vi-forward-char)
(define-key *command-keymap* "h" 'vi-backward-char)
(define-key *command-keymap* "j" 'vi-next-line)
(define-key *command-keymap* "k" 'vi-previous-line)
(define-key *command-keymap* "w" 'vi-forward-word-begin)
(define-key *command-keymap* "b" 'vi-backward-word-begin)
(define-key *command-keymap* "W" 'vi-forward-word-begin-broad)
(define-key *command-keymap* "B" 'vi-backward-word-begin-broad)
(define-key *command-keymap* "e" 'vi-forward-word-end)
(define-key *command-keymap* "E" 'vi-forward-word-end-broad)
(define-key *command-keymap* "$" 'vi-move-to-end-of-line)
(define-key *command-keymap* "^" 'vi-back-to-indentation)
(define-key *command-keymap* "{" 'backward-paragraph)
(define-key *command-keymap* "}" 'forward-paragraph)
(define-key *command-keymap* "x" 'vi-delete-next-char)
(define-key *command-keymap* "X" 'vi-delete-previous-char)
(define-key *command-keymap* "u" 'undo)
(define-key *command-keymap* "C-r" 'redo)
(define-key *command-keymap* "i" 'vi-insert)

(define-key *insert-keymap* "escape" 'vi-normal)

(define-command vi-insert () ()
  (trans-state 'insert))

(define-command vi-normal () ()
  (trans-state 'command))
