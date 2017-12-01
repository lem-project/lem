(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem.universal-argument
        :lem-vi-mode.word))
(in-package :lem-vi-mode)

(defvar *vi-keymap* (make-keymap :name '*vi-keymap* :undef-hook 'vi-command-dispatcher))
(defvar *command-keymap* (make-keymap :name '*command-keymap* :undef-hook 'vi-default-undef-hook))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap*))

(defvar *current-state*)
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
    (setf *current-state* state)
    (funcall (vi-state-function state))))

(define-vi-state command (:keymap *command-keymap*)
  (setf (element-name *modeline-element*) "[NORMAL]"))

(define-vi-state insert (:keymap *insert-keymap*)
  (message " -- INSERT --")
  (setf (element-name *modeline-element*) "[INSERT]"))

(defun on-hook ()
  (setf *modeline-element* (make-vi-modeline-element))
  (modeline-add-status-list *modeline-element*)
  (trans-state 'command))

(defun off-hook ()
  (modeline-remove-status-list *modeline-element*))

(define-global-mode vi-mode emacs-mode
  (:keymap *vi-keymap*
   :on-hook #'on-hook
   :off-hook #'off-hook))

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
(define-key *command-keymap* "x" 'vi-delete-next-char)
(define-key *command-keymap* "X" 'vi-delete-previous-char)
(define-key *command-keymap* "i" 'vi-insert)

(define-key *insert-keymap* "escape" 'vi-insert-end)

(defun call-global-command (key-seq arg)
  (let ((command (lem::keymap-find-keybind *global-keymap* key-seq)))
    (call-command command arg)))

(define-command vi-command-dispatcher (arg) ("P")
  (let* ((key-seq (last-read-key-sequence))
         (command (lem::keymap-find-keybind (vi-state-keymap *current-state*) key-seq)))
    (if command
        (call-command command arg)
        (call-global-command key-seq arg))))

(define-command vi-default-undef-hook (arg) ("P")
  (let ((key-seq (last-read-key-sequence)))
    (if (insertion-key-p key-seq)
        (undefined-key)
        (call-global-command key-seq arg))))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (forward-char n))

(define-command vi-backward-char (&optional (n 1)) ("p")
  (backward-char n))

(define-command vi-next-line (&optional (n 1)) ("p")
  (next-line n))

(define-command vi-previous-line (&optional (n 1)) ("p")
  (previous-line n))

(define-command vi-forward-word-begin (&optional (n 1)) ("p")
  (forward-word-begin (current-point) n nil))

(define-command vi-backward-word-begin (&optional (n 1)) ("p")
  (backward-word-begin (current-point) n nil))

(define-command vi-forward-word-begin-broad (&optional (n 1)) ("p")
  (forward-word-begin (current-point) n t))

(define-command vi-backward-word-begin-broad (&optional (n 1)) ("p")
  (backward-word-begin (current-point) n t))

(define-command vi-forward-word-end (&optional (n 1)) ("p")
  (forward-word-end (current-point) n nil))

(define-command vi-forward-word-end-broad (&optional (n 1)) ("p")
  (forward-word-end (current-point) n t))

(define-command vi-move-to-beginning-of-line () ()
  (move-to-beginning-of-line))

(define-command vi-move-to-end-of-line () ()
  (move-to-end-of-line))

(define-command vi-back-to-indentation () ()
  (back-to-indentation-command))

(define-command vi-delete-next-char (&optional (n 1)) ("p")
  (delete-next-char n))

(define-command vi-delete-previous-char (&optional (n 1)) ("p")
  (delete-previous-char n))

(define-command vi-insert () ()
  (trans-state 'insert))

(define-command vi-insert-end () ()
  (trans-state 'command))
