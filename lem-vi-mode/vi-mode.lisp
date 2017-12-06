(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode.mode
        :lem-vi-mode.modeline
        :lem-vi-mode.state
        :lem-vi-mode.commands
        :lem-vi-mode.ex
        :lem.universal-argument)
  (:import-from :lem-vi-mode.ex-command
                :define-ex-command)
  (:export :*command-keymap*
           :*insert-keymap*
           :*enable-hook*
           :*disable-hook*
           :define-ex-command))
(in-package :lem-vi-mode)

(defvar *command-keymap* (make-keymap :name '*command-keymap* :insertion-hook 'undefined-key))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap*))
(defvar *inactive-keymap* (make-keymap))

(define-vi-state command (:keymap *command-keymap*))

(define-vi-state insert (:keymap *insert-keymap*)
  (message " -- INSERT --"))

(define-vi-state modeline (:keymap *inactive-keymap*))

(defun minibuffer-activate-hook () (change-state 'modeline))
(defun minibuffer-deactivate-hook () (change-state 'command))

(add-hook *enable-hook*
          (lambda ()
            (initialize-vi-modeline)
            (change-state 'command)
            (add-hook *minibuffer-activate-hook* 'minibuffer-activate-hook)
            (add-hook *minibuffer-deactivate-hook* 'minibuffer-deactivate-hook)))

(add-hook *disable-hook*
          (lambda ()
            (finalize-vi-modeline)
            (remove-hook *minibuffer-activate-hook* 'minibuffer-activate-hook)
            (remove-hook *minibuffer-deactivate-hook* 'minibuffer-deactivate-hook)))

(define-command vi-end-insert () ()
  (change-state 'command)
  (vi-backward-char 1))

(define-command vi-insert () ()
  (change-state 'insert))

(define-command vi-append () ()
  (forward-char 1)
  (change-state 'insert))

(define-command vi-append-line () ()
  (move-to-end-of-line)
  (change-state 'insert))

(define-command vi-open-below () ()
  (let* ((p (current-point))
         (column (with-point ((p (current-point)))
                   (point-column (or (and (line-offset p 1)
                                          (back-to-indentation p))
                                     (line-start p))))))
    (line-end p)
    (insert-character p #\newline)
    (move-to-column p column t)
    (change-state 'insert)))

(define-command vi-open-adove () ()
  (line-start (current-point))
  (open-line 1)
  (change-state 'insert))

(define-command vi-normal () ()
  (change-state 'command))
