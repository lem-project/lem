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
        (js-patterns-no-double-quote (let ((patterns (lem-js-mode::make-tm-patterns-js)))
                                       (setf (lem/buffer/internal::patterns patterns)
                                             ;; Remove all string patterns except string interpolation literal pattern (`${}`)
                                             (remove-if (lambda (pattern)
                                                          (and (eq (lem/buffer/internal::tm-rule-name pattern)
                                                                   'lem/buffer/internal::syntax-string-attribute)
                                                               (not (typep (first (lem/buffer/internal::patterns
                                                                                   (lem/buffer/internal::tm-region-patterns pattern)))
                                                                           'lem/buffer/internal::tm-region))))
                                                        (lem/buffer/internal::patterns patterns)))
                                       ;; Add back single quote string pattern
                                       (push (make-tm-string-region "'") (lem/buffer/internal::patterns patterns))
                                       patterns)))
    (add-tm-pattern tmlanguage
                    (let ((template-keywords-with-parameters (list "v-html" "v-if" "v-bind"
                                                                   "v-for" "v-on" "v-model"
                                                                   "v-slot" "v-show" "v-text"
                                                                   "v-else-if" "v-memo"
                                                                   "@" "#" ":")))
                      (make-tm-region
                       (format nil "(~{~a~^|~})(:*)[a-zA-Z-]*(=)(\")" template-keywords-with-parameters)
                       "\""
                       :begin-captures #(nil syntax-keyword-attribute syntax-keyword-attribute syntax-keyword-attribute syntax-string-attribute)
                       :end-captures #(syntax-string-attribute)
                       :patterns js-patterns-no-double-quote)))
    (add-tm-pattern tmlanguage
                    (make-tm-match (list :sequence
                                         :word-boundary
                                         (list :alternation "v-else" "v-pre" "v-once" "v-cloak")
                                         :word-boundary)
                                   :name 'syntax-keyword-attribute))
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
