(defpackage :lem-vue-mode
  (:use :cl
        :lem
        :lem/language-mode
        :lem/language-mode-tools
        :lem-js-mode)
  (:export :*vue-mode-hook*
           :vue-mode
           :*vue-language-server-location*))

(in-package :lem-vue-mode)

(declaim (type (or pathname string) *vue-language-server-location*))
(defvar *vue-language-server-location*
  #+unix #P"/usr/lib/node_modules/@vue/language-server/"
  #+windows (merge-pathnames "npm/node_modules/@vue/language-server/" (uiop:getenv "APPDATA"))
  "The location of the language server source code files for \"@vue/language-server\"")

(defun make-tmlanguage-vue ()
  "Make the tmlanguage instance for vue-mode"
  (let ((tmlanguage (lem-html-mode::make-tmlanguage-html))
        (js-patterns (lem-js-mode::make-tm-patterns-js))
        (js-single-quote-patterns (lem-js-mode::make-tm-patterns-js)))
    (setf (lem/buffer/internal::patterns js-single-quote-patterns)
          (remove-if (lambda (pattern)
                       (eq (lem/buffer/internal::tm-rule-name pattern)
                           'lem/buffer/internal::syntax-string-attribute))
                     (lem/buffer/internal::patterns js-single-quote-patterns)))
    (push (make-tm-string-region "'") (lem/buffer/internal::patterns js-single-quote-patterns))
    (add-tm-pattern tmlanguage
                    (make-tm-region
                     ":[a-zA-Z-]+=\""
                     "\""
                     :begin-captures #(syntax-builtin-attribute)
                     :end-captures #(syntax-builtin-attribute)
                     :patterns js-single-quote-patterns))
    (add-tm-pattern tmlanguage
                    (make-tm-region
                     "(v-html|v-if|v-bind|v-for|v-on|@|#)[a-zA-Z-]*=\""
                     "\""
                     :begin-captures #(syntax-builtin-attribute)
                     :end-captures #(syntax-builtin-attribute)
                     :patterns js-single-quote-patterns))
    (add-tm-pattern tmlanguage
                    (make-tm-region
                     "{{"
                     "}}"
                     :begin-captures #(syntax-keyword-attribute)
                     :end-captures #(syntax-keyword-attribute)
                     :patterns js-patterns))
    tmlanguage))

(defun make-syntax-table-vue ()
  "Make syntax table instance and configure it for vue-mode"
  (let ((syntax-table (make-syntax-table
                       :space-chars lem-js-mode::*js-spaces*
                       :paren-pairs (list
                                     (cons #\( #\))
                                     (cons #\{ #\})
                                     (cons #\[ #\]))
                       :string-quote-chars (list #\" #\' #\`)
                       :line-comment-string "//"
                       :block-comment-pairs (list (cons "/*" "*/"))))
        (tmlanguage (make-tmlanguage-vue)))
    (set-syntax-parser syntax-table tmlanguage)
    syntax-table))

(define-major-mode vue-mode language-mode
    (:name "Vue"
     :mode-hook *vue-mode-hook*
     :syntax-table (make-syntax-table-vue)
     :keymap *vue-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-js-mode::beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-js-mode::end-of-defun))

(define-file-type ("vue") vue-mode)
