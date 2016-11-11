(in-package :cl-user)
(defpackage :lem.python
  (:use :cl :lem)
  (:export))
(in-package :lem.python)

(defvar *python-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :line-comment-preceding-char #\#))

(define-major-mode python-mode prog-mode
  (:name "python"
   :keymap *python-mode-keymap*
   :syntax-table *python-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :beginning-of-defun-function)
        'python-beginning-of-defun))

(loop :for (str symbol) :in '(("\"\"\"" :start-double-quote-docstring)
                              ("'''" :start-single-quote-docstring)) :do
  (syntax-add-region *python-syntax-table*
                     (make-syntax-test str)
                     (make-syntax-test str)
                     :attribute *syntax-string-attribute*))

(dolist (str '("and" "as" "assert" "break" "class" "continue" "def" "del"
               "elif" "else" "except" "exec" "finally" "for" "from" "global"
               "if" "import" "in" "is" "lambda" "not" "or" "pass" "print"
               "raise" "return" "try" "while" "with" "yield"))
  (syntax-add-match *python-syntax-table*
                    (make-syntax-test str :word-p t)
                    :attribute *syntax-keyword-attribute*))

(defvar *python-indent-size* 4)

(define-key *python-mode-keymap* (kbd "C-i") 'python-indent)
(define-command python-indent (n) ("p")
  (when (minusp n)
    (return-from python-indent
      (python-unindent (- n))))
  (dotimes (_ n t)
    (multiple-value-bind (start end)
        (ppcre:scan "^\\s*" (current-line-string))
      (when start
        (save-excursion (detab-line 1))
        (let ((mod (mod end *python-indent-size*)))
          (set-charpos end)
          (insert-string
           (make-string (- *python-indent-size* mod)
                        :initial-element #\space)))))))

(define-key *python-mode-keymap* (kbd "M-i") 'python-unindent)
(define-command python-unindent (n) ("p")
  (when (minusp n)
    (return-from python-unindent
      (python-indent (- n))))
  (dotimes (_ n t)
    (multiple-value-bind (start end)
        (ppcre:scan "^\\s*" (current-line-string))
      (when start
        (save-excursion (detab-line 1))
        (let ((mod (mod end *python-indent-size*)))
          (set-charpos end)
          (delete-char (- mod) nil)
          (when (plusp (- end mod))
            (delete-char (- *python-indent-size*) nil)))))))

(defun python-definition-line-p ()
  (looking-at-line "^\\s*(def|class)\\s"))

(define-command python-beginning-of-defun (n) ("p")
  (beginning-of-defun-abstract n #'python-definition-line-p))

(setq *auto-mode-alist*
      (append '(("\\.py$" . python-mode))
              *auto-mode-alist*))
