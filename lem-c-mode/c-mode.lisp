(defpackage :lem-c-mode
  (:use :cl :lem)
  (:import-from
   :lem.language-mode
   :language-mode
   :indent)
  (:export))
(in-package :lem-c-mode)

(defvar *c-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-alist '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :expr-prefix-chars '(#\, #\;)
                :expr-suffix-chars '(#\, #\;)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (lem-c-mode.grammer:make-tmlanguage-c)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode c-mode language-mode
    (:name "c"
     :keymap *c-mode-keymap*
     :syntax-table *c-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'calc-indent)
  (setf (variable-value 'indent-tabs-mode) t))

(defun calc-indent (point)
  (back-to-indentation point)
  (when (maybe-beginning-of-comment point)
    (return-from calc-indent (1+ (point-charpos point))))
  (when (in-string-p point)
    (return-from calc-indent nil))
  (when (looking-at point "\\}")
    (unless (form-offset point -1) (return-from calc-indent nil))
    (back-to-indentation point)
    (return-from calc-indent (- (point-column point) (tab-size))))
  (with-point ((start point))
    (tagbody
     :start
      (unless (line-offset point -1)
        (return-from calc-indent 0))
      (line-end point)
      (maybe-beginning-of-string-or-comment point)
      (form-offset point -1)
      (back-to-indentation point)
      (return-from calc-indent
        (cond ((and (looking-at point "^\\s*(case:|default:|do\\b|else\\b|for\\b|if\\b|switch\\b|while\\b)")
                    (not (looking-at point ".*(?:;|\\{\\})\\s*$")))
               (message "case-1")
               (or (when (looking-at point "if\\s*\\(")
                     (with-point ((point point))
                       (skip-chars-forward point (lambda (c) (not (eql c #\())))
                       (let ((column (point-column point)))
                         (when (or (null (form-offset point 1))
                                   (point< start point))
                           (1+ column)))))
                   (+ (point-column point) (tab-size))))
              ((or (looking-at point "^\\s*$")
                   (looking-at point "^\\s*(?:\\|\\||&&)"))
               (message "case-2")
               (go :start))
              ((with-point ((p point))
                 (when (looking-at p ".*\\{\\s*$")
                   (line-end p)
                   (skip-chars-backward p '(#\{))
                   (form-offset p 1)
                   (if (point< start p)
                       (+ (point-column point) (tab-size))
                       nil))))
              ((with-point ((point point))
                 (let (p)
                   (when (and (looking-at point ".*\\,\\s*$")
                              (setf p (copy-point (line-end point) :temporary))
                              (scan-lists point 1 1 t)
                              (eql #\) (character-at point -1))
                              (point< start point))
                     (message "case-4")
                     (scan-lists p -1 1 t)
                     (1+ (point-column p))))))
              (t
               (message "case-5")
               (point-column point)))))))

(pushnew (cons "\\.c$" 'c-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.h$" 'c-mode) *auto-mode-alist* :test #'equal)
