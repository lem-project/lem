(defpackage #:lem-html-mode
  (:use #:cl
        #:lem
        #:lem.language-mode
        #:lem-xml-mode)
  (:import-from #:lem-xml-mode
                #:*xml-mode-keymap*
                #:*xml-syntax-table*
                #:*xml-open-tag-p*
                #:*xml-close-tag-p*
                #:xml-calc-indent)
  (:import-from #:cl-ppcre))
(in-package #:lem-html-mode)

(defvar *html-mode-hook* '())

(define-major-mode html-mode language-mode
    (:name "html"
     :keymap *xml-mode-keymap*
     :syntax-table *xml-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'html-calc-indent)
  (run-hooks *html-mode-hook*))

(defvar *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr"))

(defun open-tag-p (string)
  (ppcre:register-groups-bind (tag-content tag-name)
      ("^<(([^/>\\s]+)[^>]*)>$" string)
    (and (not (eql (aref tag-content 0) #\!))
         (not (ppcre:scan "/\\s*$" tag-content))
         (not (find tag-name *void-elements* :test 'string-equal)))))

(defun close-tag-p (string)
  (and (ppcre:scan "^</([^/\\s<>]+)>$" string)
       t))

(defun html-calc-indent (point)
  (let ((*xml-open-tag-p* #'open-tag-p)
        (*xml-close-tag-p* #'close-tag-p))
    (xml-calc-indent point)))

(pushnew (cons "\\.html$" 'html-mode) *auto-mode-alist* :test #'equal)
