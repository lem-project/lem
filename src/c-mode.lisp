(in-package :cl-user)
(defpackage :lem.c-mode
  (:use :cl :lem :lem.prog-mode)
  (:export
   :*c-mode-keymap*
   :*c-syntax-table*
   :c-mode))
(in-package :lem.c-mode)

(defvar *c-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :escape-chars '(#\\)
   :line-comment-preceding-char #\/
   :line-comment-following-char #\/
   :block-comment-preceding-char #\/
   :block-comment-following-char #\*))

(define-major-mode c-mode prog-mode
  (:name "c"
   :keymap *c-mode-keymap*
   :syntax-table *c-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :beginning-of-defun-function)
        'c-beginning-of-defun)
  (setf (get-bvar :end-of-defun-function)
        'c-end-of-defun))

(dolist (str '("void" "char" "short" "int" "long" "float" "double" "auto"
               "static" "const" "signed" "unsigned" "extern" "volatile"
               "register" "return" "goto" "if" "else" "switch" "case"
               "default" "break" "for" "while" "do" "continue" "typedef"
               "struct" "enum" "union" "sizeof" "inline" "restrict"
               "_Bool" "_Complex" "Imaginary" "bool"))
  (syntax-add-match *c-syntax-table*
                    (make-syntax-test str :word-p t)
                    :attribute *syntax-keyword-attribute*))

(syntax-add-match *c-syntax-table*
                  (make-syntax-test "^#\\S+" :regex-p t)
                  :attribute *syntax-constant-attribute*)

;; (defvar *c-compile-command* "make")

;; (define-key *c-mode-keymap* (kbd "C-c") 'c-compile)
;; (define-command c-compile () ()
;;   )

(define-command c-beginning-of-defun (n) ("p")
  (beginning-of-defun-abstract
   n
   #'(lambda ()
       (or (looking-at-line "^{")
           (looking-at-line "^\\S[^{]*{"))))
  (when (looking-at-line "^{")
    (forward-line -1))
  t)

(define-command c-end-of-defun (n) ("p")
  (beginning-of-defun-abstract (- n) #'(lambda () (looking-at-line "^}")))
  (forward-line 1))

(setq *auto-mode-alist*
      (append '(("\\.c$" . c-mode)
                ("\\.h$" . c-mode))
              *auto-mode-alist*))
