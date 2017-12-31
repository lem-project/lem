(defpackage :lem-vi-mode.commands
  (:use :cl
        :lem
        :lem.universal-argument
        :lem.show-paren
        :lem-vi-mode.core
        :lem-vi-mode.word
        :lem-vi-mode.visual)
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
           :vi-delete
           :vi-delete-line
           :vi-yank-line
           :vi-paste-after
           :vi-paste-before
           :vi-replace-char
           :vi-move-to-matching-paren
           :vi-search-forward
           :vi-search-backward
           :vi-search-next
           :vi-search-previous
           :vi-goto-first-line
           :vi-goto-line
           :vi-write
           :vi-quit
           :vi-write-quit
           :vi-end-insert
           :vi-insert
           :vi-insert-line
           :vi-append
           :vi-append-line
           :vi-open-below
           :vi-open-adove
           :vi-normal))
(in-package :lem-vi-mode.commands)

(defvar *forward-matching-paren-offset* -1)

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

(defvar *vi-delete-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-delete () ()
    (cond (*vi-delete-recursive*
           (move-to-beginning-of-line)
           (kill-line 1)
           (throw tag t))
          ((visual-p)
           (apply-visual-range (lambda (start end) (kill-region start end))))
          (t
           (let ((command (lookup-keybind (read-key))))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-delete-recursive* t)
                       (*forward-matching-paren-offset* 0))
                   (catch tag
                     (call-command command nil)
                     (when (point/= start (current-point))
                       (kill-region start (current-point))))))))))))

(define-command vi-delete-line () ()
  (cond ((visual-block-p)
         (apply-visual-range (lambda (start end)
                               (kill-region start (line-end end)))))
        ((visual-p)
         (apply-visual-range (lambda (start end)
                               (kill-region (line-start start) (line-end end)))))
        (t
         (with-point ((start (current-point))
                      (end (current-point)))
           (kill-region start (line-end end))))))

(define-command vi-yank-line () ()
  (with-point ((start (current-point))
               (end (current-point)))
    (line-start start)
    (line-end end)
    (copy-region start end)))

(define-command vi-paste-after () ()
  (insert-character (line-end (current-point)) #\newline)
  (yank))

(define-command vi-paste-before () ()
  (line-start (current-point))
  (open-line 1)
  (yank))

(define-command vi-replace-char (c)
    ((list (key-to-char (read-key))))
  (delete-next-char 1)
  (insert-character (current-point) c))

(defun forward-matching-paren (p &optional skip)
  (with-point ((p p))
    (when (or skip (syntax-open-paren-char-p (character-at p)))
      (scan-lists p 1 0)
      (character-offset p *forward-matching-paren-offset*))))

(defun backward-matching-paren (p)
  (when (syntax-closed-paren-char-p (character-at p))
    (scan-lists (character-offset (copy-point p :temporary) 1) -1 0)))

(define-command vi-move-to-matching-paren () ()
  (alexandria:when-let ((p (or (backward-matching-paren (current-point))
                               (forward-matching-paren (current-point) t))))
    (move-point (current-point) p)))

(let ((old-forward-matching-paren)
      (old-backward-matching-paren))
  (defun on-matching-paren ()
    (setf old-forward-matching-paren (variable-value 'forward-matching-paren :global))
    (setf old-backward-matching-paren (variable-value 'backward-matching-paren :global))
    (setf (variable-value 'forward-matching-paren :global) 'forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) 'backward-matching-paren))
  (defun off-matching-paren ()
    (setf (variable-value 'forward-matching-paren :global) old-forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) old-backward-matching-paren)))

(add-hook *enable-hook* 'on-matching-paren)
(add-hook *disable-hook* 'off-matching-paren)

(define-command vi-search-forward () ()
  (lem.isearch:isearch-forward-regexp "/"))

(define-command vi-search-backward () ()
  (lem.isearch:isearch-backward-regexp "?"))

(define-command vi-search-next (n) ("p")
  (lem.isearch:isearch-next-highlight n))

(define-command vi-search-previous (n) ("p")
  (lem.isearch:isearch-prev-highlight n))

(define-command vi-goto-first-line () ()
  (move-to-beginning-of-buffer))

(define-command vi-goto-line (arg) ("P")
  (if (null arg)
      (move-to-end-of-buffer)
      (goto-line arg)))

(define-command vi-write () ()
  (lem:write-file (lem:buffer-filename (lem:current-buffer))))

(define-command vi-quit (&optional (ask t)) ((list t))
  (if (one-window-p)
      (exit-lem ask)
      (delete-current-window)))

(define-command vi-write-quit () ()
  (vi-write)
  (vi-quit nil))

(define-command vi-end-insert () ()
  (change-state 'command)
  (vi-backward-char 1))

(define-command vi-insert () ()
  (change-state 'insert))

(define-command vi-insert-line () ()
  (move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t)
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
