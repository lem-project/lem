(defpackage :lem-xml-mode
  (:use :cl :lem :lem.language-mode)
  (:import-from :cl-ppcre
                :all-matches-as-strings)
  (:export :*xml-mode-hook*))
(in-package :lem-xml-mode)

(defvar *xml-mode-hook* '())

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-xml ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region '(:sequence "<!--")
                                    '(:sequence "-->")
                                    :name 'syntax-comment-attribute)
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-region '(:sequence "'")
                                    '(:sequence "'")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns (make-tm-match "\\\\.")))
                    (make-tm-match (tokens nil '("<" "</" "<?" ">" "/>" "?>" "="))
                                   :name 'syntax-keyword-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *xml-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\< . #\>))
                :string-quote-chars '(#\" #\' #\`)
                :block-comment-pairs '(("<!--" . "-->"))))
        (tmlanguage (make-tmlanguage-xml)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode xml-mode language-mode
    (:name "xml"
     :keymap *xml-mode-keymap*
     :syntax-table *xml-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'xml-calc-indent)
  (run-hooks *xml-mode-hook*))

(defun get-line-indent (point)
  (line-start point)
  (with-point ((start point)
               (end point))
    (skip-whitespace-forward end t)
    (points-to-string start end)))

(defun move-to-previous-line (point)
  (line-start point)
  (loop do (or (line-offset point -1)
               (return nil))
        while (or (blank-line-p point)
                  (in-string-or-comment-p point))
        finally (return t)))

(defun close-tag-p (string)
  (string= "</" string :end2 2))

(defun calc-indent-from-prev (string)
  (let* ((tags (ppcre:all-matches-as-strings "<.*?>" string))
         (tags (member-if (complement #'close-tag-p) tags)))
    (- (length tags) (* 2 (count-if #'close-tag-p tags)))))

(defun calc-indent-from-current (string)
  (let ((tags (ppcre:all-matches-as-strings "<.*?>" string)))
    (- (length (member-if (complement #'close-tag-p) tags))
       (length tags))))

(defun xml-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (+ (with-point ((prev point))
         (if (move-to-previous-line prev)
             (with-point ((start prev)
                          (end prev))
               (line-start start)
               (line-end end)
               (+ (length (get-line-indent prev))
                  (* (calc-indent-from-prev (points-to-string start end))
                     tab-width)))
             0))
       (with-point ((start point)
                    (end point))
         (line-start start)
         (line-end end)
         (* (calc-indent-from-current (points-to-string start end))
            tab-width)))))

(pushnew (cons "\\.xml$" 'xml-mode) *auto-mode-alist* :test #'equal)
