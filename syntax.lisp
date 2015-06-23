(in-package :lem)

(defstruct syntax-table
  space-chars
  symbol-chars
  paren-alist
  string-quote-chars
  escape-chars
  expr-prefix-chars
  comment-starter-chars
  comment-ender-chars)

(defvar *current-syntax*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\>)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\")
   :escape-chars '(#\\)
   :expr-prefix-chars '(#\' #\, #\@ #\#)
   :comment-starter-chars '(#\;)
   :comment-ender-chars '(#\newline)))

(defun syntax-word-char-p (c)
  (not
   (or
    (syntax-space-char-p c)
    (syntax-symbol-char-p c)
    (syntax-open-paren-char-p c)
    (syntax-closed-paren-char-p c)
    (syntax-string-quote-char-p c)
    (syntax-escape-char-p c)
    (syntax-expr-prefix-char-p c)
    (syntax-comment-starter-char-p c)
    (syntax-comment-ender-char-p c))))

(defun syntax-space-char-p (c)
  (member c (syntax-table-space-chars *current-syntax*)))

(defun syntax-symbol-char-p (c)
  (member c (syntax-table-symbol-chars *current-syntax*)))

(defun syntax-open-paren-char-p (c)
  (assoc c (syntax-table-paren-alist *current-syntax*)))

(defun syntax-closed-paren-char-p (c)
  (rassoc c (syntax-table-paren-alist *current-syntax*)))

(defun syntax-pair-open-paren (c)
  (car (rassoc c (syntax-table-paren-alist *current-syntax*))))

(defun syntax-pair-closed-paren (c)
  (cdr (assoc c (syntax-table-paren-alist *current-syntax*))))

(defun syntax-string-quote-char-p (c)
  (member c (syntax-table-string-quote-chars *current-syntax*)))

(defun syntax-escape-char-p (c)
  (member c (syntax-table-escape-chars *current-syntax*)))

(defun syntax-expr-prefix-char-p (c)
  (member c (syntax-table-expr-prefix-chars *current-syntax*)))

(defun syntax-comment-starter-char-p (c)
  (member c (syntax-table-comment-starter-chars *current-syntax*)))

(defun syntax-comment-ender-char-p (c)
  (member c (syntax-table-comment-ender-chars *current-syntax*)))
