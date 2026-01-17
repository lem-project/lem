(defpackage :lem-typescript-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools)
  (:export :typescript-mode
           :tsx-mode))
(in-package :lem-typescript-mode)

(defvar *typescript-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\Space #\Tab #\Newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (lem-js-mode::make-tmlanguage-js)))
    (set-syntax-parser table tmlanguage)
    table))

(defvar *tsx-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\Space #\Tab #\Newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (lem-js-mode::make-tmlanguage-js)))
    (set-syntax-parser table tmlanguage)
    table))

(defun typescript-tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for TypeScript."
  (asdf:system-relative-pathname :lem-typescript-mode "tree-sitter/highlights.scm"))

(define-major-mode typescript-mode lem/language-mode:language-mode
    (:name "TypeScript"
     :keymap *typescript-mode-keymap*
     :syntax-table *typescript-syntax-table*
     :mode-hook *typescript-mode-hook*
     :formatter 'lem-js-mode::prettier)
  (lem-tree-sitter:enable-tree-sitter-for-mode
   *typescript-syntax-table* "typescript" (typescript-tree-sitter-query-path))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-js-mode::beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-js-mode::end-of-defun))

(define-major-mode tsx-mode lem/language-mode:language-mode
    (:name "TSX"
     :keymap *tsx-mode-keymap*
     :syntax-table *tsx-syntax-table*
     :mode-hook *tsx-mode-hook*
     :formatter 'lem-js-mode::prettier)
  (lem-tree-sitter:enable-tree-sitter-for-mode
   *tsx-syntax-table* "tsx" (typescript-tree-sitter-query-path))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-js-mode::beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-js-mode::end-of-defun))

(define-file-type ("ts") typescript-mode)
(define-file-type ("tsx") tsx-mode)
