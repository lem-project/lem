(defpackage :smart-parens-mode
  (:use :cl :lem)
  (:export :smart-parens-mode))

(in-package :smart-parens-mode)

(define-minor-mode smart-parens-mode
    (:name "smart-parens-mode"
     :keymap *smart-parens-keymap*))

(defun bolp (point)
  (zerop (point-charpos point)))

(defun eolp (point)
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
            (1- len)))))

(defun integer-char-p (char)
  (< (char-code #\0) (char-code char) (char-code #\9)))

(defun sharp-literal-p (char point)
  (with-point ((p point))
    (character-offset p -1)
    (and (character-at p)
         (char-equal (character-at p) char)
         (eql (character-at p -1) #\#))))

(defun sharp-n-literal-p (char point)
  (with-point ((p point))
    (character-offset p -1)
    (when (char-equal char (character-at p))
      (character-offset p -1)
      (skip-chars-backward p #'integer-char-p)
      (and (integer-char-p (character-at p))
           (eql (character-at p -1) #\#)))))

(defparameter *non-space-following-chars*
  '(#\Space #\( #\' #\` #\, #\[ #\{))

(defparameter *non-space-preceding-chars*
  '(#\Space #\) #\] #\}))

(defun non-space-following-context-p (&optional (p (current-point)))
  (or (bolp p)
      (find (character-at p -1)
            *non-space-following-chars*)
      (eql (character-at p -1) #\#)
      (and (eql (character-at p -1) #\@)
           (eql (character-at p -2) #\,))
      (sharp-literal-p #\' p)
      (sharp-literal-p #\. p)
      (sharp-literal-p #\S p)
      (sharp-literal-p #\C p)
      (sharp-literal-p #\+ p)
      (sharp-literal-p #\- p)
      (sharp-n-literal-p #\A p)
      (sharp-n-literal-p #\= p)))

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

(define-command smart-parens-insert-paren () ()
  (editor-insert-pair #\( #\)))

(define-command smart-parens-insert-bracket () ()
  (editor-insert-pair #\[ #\]))

(define-command smart-parens-insert-brace () ()
  (editor-insert-pair #\{ #\}))

(define-command smart-parens-insert-double-quote () ()
  (editor-insert-pair #\" #\"))

(define-command smart-parens-insert-single-quote () ()
  (editor-insert-pair #\' #\'))

(define-key *smart-parens-keymap* "\"" 'smart-parens-insert-double-quote)
(define-key *smart-parens-keymap* "'" 'smart-parens-insert-single-quote)
(define-key *smart-parens-keymap* "(" 'smart-parens-insert-paren)
(define-key *smart-parens-keymap* "[" 'smart-parens-insert-bracket)
(define-key *smart-parens-keymap* "{" 'smart-parens-insert-brace)
