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
                  (#\{ . #\}))
   :string-quote-chars '(#\" #\')
   :line-comment-preceding-char #\-
   :line-comment-following-char #\-))

(define-major-mode lua-mode prog-mode
  (:name "lua"
   :keymap *lua-mode-keymap*
   :syntax-table *lua-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :calc-indent-function) 'lua-calc-indent)
  (setf (get-bvar :forward-sexp-function) 'lua-forward-sexp))

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
                     :attr :string-attr
                     :tag :long-string))

(loop
  :for n :from 0 :to 10
  :for str1 := (format nil "--[~a[" (make-string n :initial-element #\=))
  :for str2 := (format nil "]~a]" (make-string n :initial-element #\=))
  :do
  (syntax-add-region *lua-syntax-table*
                     (make-syntax-test str1)
                     (make-syntax-test str2)
                     :attr :comment-attr
                     :tag :comment))

(defun skip-space-forward ()
  (loop
    (skip-chars-forward '(#\space #\tab #\newline))
    (unless (and (not (eq :comment (syntax-preceding-tag)))
                 (eq :comment (syntax-following-tag))
                 (syntax-forward-search-tag-end :comment))
      (return t))))

(defun skip-space-backward ()
  (loop
    (skip-chars-backward '(#\space #\tab #\newline))
    (unless (and (not (eq :comment (syntax-before-tag 1)))
                 (eq :comment (syntax-before-tag 2))
                 (syntax-backward-search-tag-start :comment))
      (return t))))

(defun lua-forward-sexp-1 (n)
  (cond ((and (= n 1)
              (not (eq :long-string (syntax-preceding-tag)))
              (eq :long-string (syntax-following-tag)))
         (syntax-forward-search-tag-end :long-string))
        ((and (= n -1)
              (not (eq :long-string (syntax-before-tag 1)))
              (eq :long-string (syntax-before-tag 2)))
         (syntax-backward-search-tag-start :long-string))
        (t
         (raw-forward-sexp n))))

(defun lua-forward-sexp (n)
  (let ((point (point)))
    (dotimes (_ (abs n) t)
      (unless (and (if (plusp n)
                       (skip-space-forward)
                       (skip-space-backward))
                   (lua-forward-sexp-1 (if (plusp n) 1 -1)))
        (point-set point)
        (return nil)))))

(defun lua-definition-line-p ()
  (looking-at-line "^(function|local)\\s"))

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
   (end-of-line)
   (skip-chars-backward '(#\space #\tab))
   (eql #\, (preceding-char))))

(defun scan-line ()
  (let ((string (save-excursion
                 (region-string (progn (beginning-of-line) (point))
                                (progn (end-of-line) (point)))))
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

(defun lua-calc-indent-1 ()
  (cond ((unfinished-line-p)
         (loop :repeat 100 :while (backward-sexp 1 t))
         (current-column))
        ((looking-at-line ".*?;\\s*$")
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
         (current-column))))

(defun lua-calc-indent ()
  (let ((end-line-p (contains-word-p "end" "else" "elseif" "until"))
        (n (save-excursion
            (beginning-of-line)
            (skip-backward-comment-and-space)
            (lua-calc-indent-1))))
    (if end-line-p
        (- n 8)
        n)))

(setq *auto-mode-alist*
      (append '(("\\.lua$" . lua-mode))
              *auto-mode-alist*))
