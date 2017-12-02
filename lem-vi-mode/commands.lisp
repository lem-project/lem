(defpackage :lem-vi-mode.commands
  (:use :cl
        :lem
        :lem-vi-mode.word
        :lem.universal-argument)
  (:export :vi-move-to-beginning-of-line
           :vi-forward-char
           :vi-backward-char
           :vi-next-line
           :vi-previous-line
           :vi-forward-word-begin
           :vi-backward-word-begin
           :vi-forward-word-begin-broad
           :vi-backward-word-begin-broad
           :vi-forward-word-end
           :vi-forward-word-end-broad
           :vi-move-to-beginning-of-line
           :vi-move-to-end-of-line
           :vi-back-to-indentation
           :vi-delete-next-char
           :vi-delete-previous-char))
(in-package :lem-vi-mode.commands)

(defun eolp (point)
  (= (point-charpos point)
     (1- (length (line-string point)))))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n p)
      (if (eolp p)
          (return)
          (character-offset p 1)))))

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
