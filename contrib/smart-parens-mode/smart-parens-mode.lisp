(defpackage :lem-smart-parens-mode
  (:use :cl :lem)
  (:export :smart-parens-mode))

(in-package :lem-smart-parens-mode)

(define-minor-mode smart-parens-mode
    (:name "smart-parens-mode"
     :keymap *smart-parens-keymap*))

(defun editor-insert-pair (open close)
  (let ((p (current-point)))
    (cond ((in-string-or-comment-p p)
           (insert-character p open))
          ((syntax-escape-point-p p 0)
           (insert-character p open))
          (t
           (unless (non-space-following-context-p p)
             (insert-character p #\Space))
           (insert-character p open)
           (insert-character p close)
           (unless (or (eolp p) (find (character-at p) *non-space-preceding-chars*))
             (insert-character p #\Space)
             (character-offset p -1))
           (character-offset p -1)))))

(define-command paredit-insert-paren () ()
  (editor-insert-pair #\( #\)))

(define-command paredit-insert-bracket () ()
  (editor-insert-pair #\[ #\]))

(define-command paredit-insert-brace () ()
  (editor-insert-pair #\{ #\}))

(define-command insert-double-quote () ()
  (editor-insert-pair #\" #\"))

(define-command insert-single-quote () ()
  (editor-insert-pair #\' #\'))

(define-key *smart-parens-keymap* "\"" 'insert-double-quote)
(define-key *smart-parens-keymap* "'" 'insert-single-quote)
(define-key *smart-parens-keymap* "(" 'insert-paren)
(define-key *smart-parens-keymap* "[" 'insert-bracket)
(define-key *smart-parens-keymap* "{" 'insert-brace)
