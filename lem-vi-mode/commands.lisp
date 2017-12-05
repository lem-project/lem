(defpackage :lem-vi-mode.commands
  (:use :cl
        :lem
        :lem.universal-argument
        :lem-vi-mode.word)
  (:export :vi-move-to-beginning-of-line/universal-argument-0
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
           :vi-delete-previous-char
           :vi-move-to-matching-paren))
(in-package :lem-vi-mode.commands)

(defun bolp (point)
  (zerop (point-charpos point)))

(defun eolp (point)
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
            (1- len)))))

(defun goto-bol (point)
  (line-start point))

(defun goto-eol (point)
  (line-end point)
  (unless (bolp point)
    (character-offset point -1)))

(defun empty-line (point)
  (zerop (length (line-string point))))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (eolp p)
          (return)
          (character-offset p 1)))))

(define-command vi-backward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (bolp p)
          (return)
          (character-offset p -1)))))

(defun fall-within-line (point)
  (when (eolp point)
    (goto-eol point)))

(define-command vi-next-line (&optional (n 1)) ("p")
  (next-line n)
  (fall-within-line (current-point)))

(define-command vi-previous-line (&optional (n 1)) ("p")
  (previous-line n)
  (fall-within-line (current-point)))

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
  (goto-bol (current-point)))

(define-command vi-move-to-end-of-line () ()
  (goto-eol (current-point)))

(define-command vi-back-to-indentation () ()
  (back-to-indentation-command))

(define-command vi-delete-next-char (&optional (n 1)) ("p")
  (unless (empty-line (current-point))
    (delete-next-char n)
    (fall-within-line (current-point))))

(define-command vi-delete-previous-char (&optional (n 1)) ("p")
  (unless (bolp (current-point))
    (delete-previous-char n)))

(define-command vi-move-to-matching-paren () ()
  (let* ((p (current-point))
         (c (character-at p)))
    (cond ((syntax-open-paren-char-p c)
           (scan-lists p 1 0)
           (character-offset p -1))
          ((syntax-closed-paren-char-p c)
           (scan-lists (character-offset p 1) -1 0)))))
