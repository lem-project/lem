;; -*- Mode: LISP; Package: LEM.LUA-MODE -*-

(defpackage :lem.lua-mode
  (:use :cl :lem)
  (:export))

(in-package :lem.lua-mode)

(defvar *lua-mode-keymap* (make-keymap))

(defvar *lua-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :line-comment-preceding-char #\-
   :line-comment-following-char #\-))

(define-major-mode lua-mode prog-mode
  (:name "lua"
   :keymap *lua-mode-keymap*
   :syntax-table *lua-syntax-table*)
  (buffer-put (window-buffer) :enable-syntax-highlight t)
  (buffer-put (window-buffer) :calc-indent-function 'lua-calc-indent))

(dolist (str '("and" "break" "do" "else" "elseif" "end" "false" "for"
               "goto" "if" "in" "local" "nil" "not" "or"
               "repeat" "return" "then" "true" "until" "while"))
  (syntax-add-match *lua-syntax-table*
                    (make-syntax-test str :word-p t)
                    :attr :keyword-attr))

(syntax-add-match *lua-syntax-table*
                  (make-syntax-test "function" :word-p t)
                  :attr :keyword-attr
                  :matched-symbol :function-start
                  :symbol-tov 1)

(syntax-add-match *lua-syntax-table*
                  (make-syntax-test "[a-zA-Z0-9_\\.:]+" :regex-p t)
                  :test-symbol :function-start
                  :attr :function-name-attr)

(loop
  :for n :from 0 :to 10
  :for str1 := (format nil "[~a[" (make-string n :initial-element #\=))
  :for str2 := (format nil "]~a]" (make-string n :initial-element #\=))
  :do
  (syntax-add-region *lua-syntax-table*
                     (make-syntax-test str1)
                     (make-syntax-test str2)
                     :attr :string-attr))

(loop
  :for n :from 0 :to 10
  :for str1 := (format nil "--[~a[" (make-string n :initial-element #\=))
  :for str2 := (format nil "]~a]" (make-string n :initial-element #\=))
  :do
  (syntax-add-region *lua-syntax-table*
                     (make-syntax-test str1)
                     (make-syntax-test str2)
                     :attr :comment-attr))

(defun lua-definition-line-p ()
  (looking-at "^(function|local)\\s"))

(define-key *lua-mode-keymap* (kbd "C-M-a") 'lua-beginning-of-defun)
(define-command lua-beginning-of-defun (n) ("p")
  (beginning-of-defun-abstract n #'lua-definition-line-p))

(define-key *lua-mode-keymap* (kbd "C-M-e") 'lua-end-of-defun)
(define-command lua-end-of-defun (n) ("p")
  (beginning-of-defun-abstract (- n) #'lua-definition-line-p))

(defun skip-backward-comment-and-space ()
  (backward-sexp 1 t))

(defun unfinished-line-p ()
  (save-excursion
   (let ((max-point (progn (end-of-line) (point))))
     (beginning-of-line)
     (loop
       (unless (forward-sexp 1 t)
         (return t))
       (when (point<= max-point (point))
         (return nil))))))

(defun scan-line ()
  (let ((string (region-string (progn (beginning-of-line) (point))
                               (progn (end-of-line) (point))))
        (tokens))
    (ppcre:do-matches-as-strings (tok "\\w+|;|\".*?[^\\\\]?\"|'.*?[^\\\\]'" string)
      (if (equal tok ";")
          (setq tokens nil)
          (push tok tokens)))
    tokens))

(defun contains-word-p (&rest words)
  (let ((tokens (scan-line)))
    (dolist (word words)
      (when (find word tokens :test #'equal)
        (return t)))))

(defun lua-calc-indent ()
  (let ((end-line-p (or (contains-word-p "end" "else"))))
    (save-excursion
     (beginning-of-line)
     (cond ((not (skip-backward-comment-and-space))
            0)
           (end-line-p
            (back-to-indentation)
            (- (current-column) 8))
           ((unfinished-line-p)
            (loop :while (backward-sexp 1 t))
            (current-column))
           ((looking-at ".*?;\\s*$")
            (back-to-indentation)
            (current-column))
           ((or (contains-word-p "do" "then" "else")
                (and (not (contains-word-p "end"))
                     (contains-word-p "function")))
            (back-to-indentation)
            (+ (current-column) 8))
           ((or (contains-word-p "return"))
            (back-to-indentation)
            (current-column))
           (t
            (back-to-indentation)
            (current-column))))))

(setq *auto-mode-alist*
      (append '(("\\.lua$" . lua-mode))
              *auto-mode-alist*))
