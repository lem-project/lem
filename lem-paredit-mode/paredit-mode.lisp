#|
link : http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html
|#

(defpackage :lem-paredit-mode
  (:use :cl
        :lem
        :lem-vi-mode.word)
  (:export :paredit-mode
           :paredit-forward
           :paredit-backward
           :paredit-insert-paren
           :paredit-backward-delete
           :paredit-close-parenthesis
           :paredit-slurp
           :paredit-barf
           :paredit-splice
           :*paredit-mode-keymap*))
(in-package :lem-paredit-mode)

(define-minor-mode paredit-mode
    (:name "paredit"
     :keymap *paredit-mode-keymap*))

(defun move-to-word-end (q)
  (loop while (not (syntax-space-char-p (character-at q)))
        do (character-offset q 1)))

(defun backward-open-paren-char-p (p)
  (with-point ((q p))
    (skip-whitespace-backward q)
    (syntax-open-paren-char-p (character-at q))))

(define-command paredit-forward (&optional (n 1)) ("p")
  (forward-sexp n))

(define-command paredit-backward (&optional (n 1)) ("p")
  (backward-sexp n))

(define-command paredit-insert-paren () ()
  (let ((p (current-point)))
    (dolist (c '(#\( #\)))
      (insert-character p c))
    (character-offset p -1)))

(define-command paredit-backward-delete (&optional (n 1)) ("p")
  (when (< 0 n)
    (with-point ((p (current-point)))
      (character-offset p -1)
      (case (character-at p)
        (#\( (when (char= (character-at p 1) #\))
               (delete-next-char)
               (delete-previous-char)))
        (#\) (backward-char))
        (otherwise
         (delete-previous-char))))
    (paredit-backward-delete (1- n))))

(define-command paredit-close-parenthesis () ()
  (with-point ((p (current-point)))
    (case (character-at p)
      (#\) (forward-char))
      (otherwise
       (handler-case (scan-lists p 1 1)
         (error ()
           (insert-character p #\))
           (return-from paredit-close-parenthesis)))
       (with-point ((new-p p))
         (character-offset new-p -1)
         (move-point (current-point) new-p)
         (with-point ((p new-p))
           (skip-whitespace-backward p)
           (delete-between-points p new-p)))))))

(define-command paredit-slurp () ()
  (with-point ((origin (current-point))
               (p (current-point)))
    (scan-lists p -1 1)
    (when (syntax-open-paren-char-p (character-at p))
      (scan-lists p 1 0)
      (character-offset p -1)
      (with-point ((kill-point p))
        (character-offset p 1)
        (skip-whitespace-forward p)
        (when (not (syntax-closed-paren-char-p (character-at p)))
          (with-point ((yank-point p :left-inserting))
            (if (syntax-open-paren-char-p (character-at p))
                (scan-lists yank-point 1 0)
                (move-to-word-end yank-point))
            (kill-ring-new)
            (with-point ((q kill-point))
              (character-offset q 1)
              (kill-region kill-point q)
              (move-point (current-point) yank-point)
              (yank)
              (move-point (current-point) origin)
              (indent-region origin yank-point))))))))

(define-command paredit-barf () ()
  (with-point ((origin (current-point) :right-inserting)
               (p (current-point)))
    (scan-lists p -1 1)
    (when (syntax-open-paren-char-p (character-at p))
      (scan-lists p 1 0)
      (character-offset p -2)
      (with-point ((yank-point p))
        (if (syntax-closed-paren-char-p (character-at p))
            (scan-lists yank-point -1 1)
            (backward-word-begin yank-point 1 t))
        (move-point (current-point) yank-point)
        (skip-whitespace-backward yank-point)
        (kill-ring-new)
        (with-point ((q p))
          (character-offset p 1)
          (character-offset q 2)
          (kill-region p q)
          (move-point (current-point) yank-point)
          (yank)
          (move-point (current-point) origin)
          (indent-region origin p))))))

(define-command paredit-splice () ()
  (with-point ((origin (current-point) :right-inserting)
               (start (current-point)))
    (scan-lists start -1 1)
    (when (syntax-open-paren-char-p (character-at start))
      (with-point ((end start))
        (scan-lists start 1 0)
        (character-offset start -2)
        (delete-character start)
        (delete-character end)
        (indent-region start end)))))

(loop for (k . f) in '((forward-sexp . paredit-forward)
                       (backward-sexp . paredit-backward)
                       ("(" . paredit-insert-paren)
                       (")" . paredit-close-parenthesis)
                       (delete-previous-char . paredit-backward-delete)
                       ("C-Right" . paredit-slurp)
                       ("C-Left" . paredit-barf)
                       ("M-s" . paredit-splice))
      do (define-key *paredit-mode-keymap* k f))
