(defpackage :lem-c-mode
  (:use :cl :lem)
  (:import-from
   :lem.language-mode
   :language-mode
   :indent)
  (:export :*c-mode-hook*))
(in-package :lem-c-mode)

(defvar *c-mode-hook* '())

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
  (setf (variable-value 'indent-tabs-mode) t)
  (run-hooks *c-mode-hook*))

(defun c-beginning-of-defun (point)
  (loop
    (line-start point)
    (when (looking-at point "^\\w[^=(]*\\(.*\\)")
      (return point))
    (unless (line-offset point -1)
      (return point))))

(defvar *indent-line-function* nil)

(defun %indent (p indent)
  (when *indent-line-function*
    (funcall *indent-line-function* p indent)))

(defun unbalanced-p (state)
  (if (member #\( (pps-state-paren-stack state)) t nil))

(defun c-indent-line (p indent)
  (let ((tab-width (variable-value 'tab-width :default p)))
    (back-to-indentation p)
    (loop :while (end-line-p p)
          :do (%indent p indent)
          :do (if (line-offset p 1)
                  (back-to-indentation p)
                  (return-from c-indent-line nil)))
    (when (looking-at p "\\}")
      (decf indent tab-width)
      (character-offset p 1)
      (skip-whitespace-forward p t))
    (let ((word (looking-at p "\\w+"))
          (state)
          (unbalanced-flag nil))
      (when word
        (character-offset p (length word))
        (skip-whitespace-forward p t))
      (with-point ((start p))
        (cond
          ((unbalanced-p (setf state
                               (parse-partial-sexp (copy-point start :temporary)
                                                   (line-end p))))
           (setf unbalanced-flag t)
           (%indent p indent)
           (loop
             (scan-lists p -1 1)
             (when (eql #\( (character-at p)) (return)))
           (let ((indent1 (1+ (point-column p))))
             (loop
               (unless (line-offset p 1) (return-from c-indent-line nil))
               (%indent p indent1)
               (unless (unbalanced-p (setf state
                                           (parse-partial-sexp (copy-point start :temporary)
                                                               (line-end p))))
                 (return)))))
          ((and word (ppcre:scan "^(?:case|default)$" word))
           (%indent p (- indent tab-width)))
          (t
           (%indent p indent)
           (with-point ((tmp (line-end p)))
             (when (and (search-forward (line-start p) "?" tmp)
                        (not (in-string-or-comment-p p))
                        (not (syntax-escape-char-p (character-at p -2))))
               (line-start tmp)
               (when (and (not (unbalanced-p (parse-partial-sexp tmp p)))
                          (not (and (ppcre:scan "[^\\\\]?;\\s*$" (line-string p))
                                    (not (in-string-or-comment-p (line-end p))))))
                 (loop
                   (unless (line-offset p 1) (return-from c-indent-line nil))
                   (c-indent-line p (+ indent tab-width))
                   (when (and (ppcre:scan "[^\\\\]?;\\s*$" (line-string p))
                              (not (in-string-or-comment-p (line-end p))))
                     (return)))))))))
      (when (eql #\{ (car (pps-state-paren-stack state)))
        (return-from c-indent-line (+ indent tab-width)))
      (when (and word (ppcre:scan "^(?:do|else|for|if|switch|while)$" word)
                 (not (and (not unbalanced-flag) (ppcre:scan "[};]\\s*$" (line-string p)))))
        (unless (line-offset p 1) (return-from c-indent-line nil))
        (c-indent-line p (+ indent tab-width))
        (return-from c-indent-line indent))
      (return-from c-indent-line indent))))

(defun calc-indent-region (start end)
  (with-point ((p start))
    (let ((indent (point-column (back-to-indentation p))))
      (loop
        (let ((next-indent (c-indent-line p indent)))
          (unless next-indent (return))
          (unless (line-offset p 1) (return))
          (unless (point< start end) (return))
          (setf indent next-indent))))))

(defun calc-indent (point)
  (with-point ((start point))
    (line-offset start -1)
    (c-beginning-of-defun start)
    (let ((*indent-line-function*
           (lambda (p indent)
             (when (same-line-p point p)
               (return-from calc-indent indent)))))
      (calc-indent-region start point))))

(pushnew (cons "\\.c$" 'c-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.h$" 'c-mode) *auto-mode-alist* :test #'equal)
