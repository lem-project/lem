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

(define-major-mode vue-mode language-mode
    (:name "Vue"
     :mode-hook *vue-mode-hook*
     :keymap *vue-mode-keymap*
     :formatter 'lem-js-mode::prettier
     :syntax-table lem-js-mode::*js-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'js-calc-indent
        (variable-value 'line-comment) "//"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(define-file-type ("vue") vue-mode)
