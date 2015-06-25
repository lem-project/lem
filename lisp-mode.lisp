(in-package :lem)

(defvar *special-indent-symbols*
  '(block
     let*
     catch
     eval-when
     symbol-macrolet
     flet
     macrolet
     tagbody
     multiple-value-prog1
     if
     progn
     unwind-protect
     labels
     progv
     let
     defun
     defmacro
     lambda
     when
     unless
     dotimes
     dolist
     ))

(defvar *lisp-mode-keymap*
  (make-keymap "lisp" 'undefined-key *global-keymap*))

(define-mode lisp-mode
  :name "lisp-mode"
  :keymap *lisp-mode-keymap*
  :syntax-table (make-syntax-table
                 :space-chars '(#\space #\tab #\newline)
                 :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\>)
                 :paren-alist '((#\( . #\))
                                (#\[ . #\])
                                (#\{ . #\}))
                 :string-quote-chars '(#\")
                 :escape-chars '(#\\)
                 :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
                 :comment-starter-chars '(#\;)
                 :comment-ender-chars '(#\newline)))

(define-key *lisp-mode-keymap* "C-i" 'lisp-indent-line)
(defcommand lisp-indent-line () ()
  (beginning-of-line)
  (delete-while-whitespaces t)
  (let (col
        flag
        (point (point)))
    (do ()
        ((not (backward-sexp))
         (setq col (window-cur-col))
         (let ((point (point)))
           (when (= (window-cur-linum)
                    (let ((point (point)))
                      (forward-sexp)
                      (skip-chars-forward 'syntax-space-char-p)
                      (prog1 (window-cur-linum)
                             (point-set point))))
             (forward-sexp 2)
             (backward-sexp 1)
             (setq flag (window-cur-col)))
           (point-set point))))
    (let ((chars))
      (skip-chars-forward
       (lambda (c)
         (when (syntax-symbol-char-p c)
           (push c chars)
           t)))
      (point-set point)
      (let ((first (intern (string-upcase (coerce (nreverse chars) 'string)))))
        (if (member first *special-indent-symbols*)
          (insert-char #\space (1+ col))
          (if flag
            (insert-char #\space flag)
            (insert-char #\space col)))))))
