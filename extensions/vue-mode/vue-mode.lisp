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
  (lem-html-mode::make-tmlanguage-html))

(defparameter *vue-syntax-table*
  (let ((syntax-table (make-syntax-table
                       :space-chars lem-js-mode::*js-spaces*
                       :paren-pairs '((#\( . #\))
                                      (#\{ . #\})
                                      (#\[ . #\]))
                       :string-quote-chars '(#\" #\' #\`)
                       :line-comment-string "//"
                       :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-vue)))
    (set-syntax-parser syntax-table tmlanguage)
    syntax-table))

(define-major-mode vue-mode language-mode
    (:name "Vue"
     :mode-hook *vue-mode-hook*
     :syntax-table *vue-syntax-table*
     :keymap *vue-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'lem-js-mode::js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'lem-js-mode::beginning-of-defun
        (variable-value 'end-of-defun-function) 'lem-js-mode::end-of-defun))

(define-file-type ("vue") vue-mode)
