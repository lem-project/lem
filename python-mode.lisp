;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(defvar *python-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :line-comment-preceding-char #\#))

(define-major-mode python-mode nil
  (:name "python"
   :keymap *python-mode-keymap*
   :syntax-table *python-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t))

(loop :for (str symbol) :in '(("\"\"\"" :start-double-quote-docstring)
                              ("'''" :start-single-quote-docstring)) :do
  (syntax-add-region *python-syntax-table*
                     (make-syntax-test str)
                     (make-syntax-test str)
                     :attr :string-attr))

(dolist (str '("and" "as" "assert" "break" "class" "continue" "def" "del"
               "elif" "else" "except" "exec" "finally" "for" "from" "global"
               "if" "import" "in" "is" "lambda" "not" "or" "pass" "print"
               "raise" "return" "try" "while" "with" "yield"))
  (syntax-add-match *python-syntax-table*
                    (make-syntax-test str :word-p t)
                    :attr :keyword-attr))

(defvar *python-indent-size* 4)

(define-key *python-mode-keymap* (kbd "C-i") 'python-indent)
(define-command python-indent (n) ("p")
  (when (minusp n)
    (return-from python-indent
      (python-unindent (- n))))
  (dotimes (_ n t)
    (multiple-value-bind (start end)
        (ppcre:scan "^\\s*"
                    (buffer-line-string (current-buffer)
                                        (current-linum)))
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
        (ppcre:scan "^\\s*"
                    (buffer-line-string (current-buffer)
                                        (current-linum)))
      (when start
        (save-excursion (detab-line 1))
        (let ((mod (mod end *python-indent-size*)))
          (set-charpos end)
          (backward-delete-char mod t)
          (when (plusp (- end mod))
            (backward-delete-char *python-indent-size* t)))))))

(defun python-definition-line-p ()
  (looking-at-line "^\\s*(def|class)\\s"))

(define-key *python-mode-keymap* (kbd "C-M-a") 'python-beginning-of-defun)
(define-command python-beginning-of-defun (n) ("p")
  (beginning-of-defun-abstract n #'python-definition-line-p))

(define-key *python-mode-keymap* (kbd "C-M-e") 'python-end-of-defun)
(define-command python-end-of-defun (n) ("p")
  (beginning-of-defun-abstract (- n) #'python-definition-line-p))

(setq *auto-mode-alist*
      (append '(("\\.py$" . python-mode))
              *auto-mode-alist*))
