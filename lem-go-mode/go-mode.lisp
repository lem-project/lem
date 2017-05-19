(in-package :cl-user)
(defpackage :lem-go-mode
  (:use :cl :lem)
  (:import-from
   :lem.tmlanguage
   :load-tmlanguage)
  (:import-from
   :lem.language-mode
   :language-mode
   :indent)
  (:export))
(in-package :lem-go-mode)

(defvar *go-tab-width* 8)

(defvar *go-builtin*
  '("append" "cap"   "close"   "complex" "copy"
    "delete" "imag"  "len"     "make"    "new"
    "panic"  "print" "println" "real"    "recover"))

(defvar *go-keywords*
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var"))

(defvar *go-constants*
  '("nil" "true" "false" "iota"))

(defvar *go-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-alist '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (load-tmlanguage
                     (merge-pathnames "go.json"
                                      (merge-pathnames "lem-go-mode/"
                                                       (asdf:system-source-directory :lem))))))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode go-mode language-mode
    (:name "go"
     :keymap *go-mode-keymap*
     :syntax-table *go-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'go-calc-indent)
  (setf (variable-value 'indent-tabs-mode) t))

(defun following-word (point)
  (points-to-string point
		    (form-offset (copy-point point :temporary) 1)))

(defun semicolon-p (point)
  (let ((c (character-at point -1)))
    (case c
      ((#\;) t)
      ((#\' #\" #\`) t)
      ((#\+ #\-)
       (character-offset point -1)
       (eql c (character-at point -1)))
      ((#\) #\] #\}) t)
      (t
       (and (/= 0 (skip-chars-backward point #'syntax-word-char-p))
            #-(and)(not (member (following-word point)
                                '("break" "continue" "fallthrough" "return")
                                :test #'string=)))))))

(defun go-calc-indent (point)
  (save-excursion
    (back-to-indentation point)
    (cond ((maybe-beginning-of-comment point)
           (1+ (point-column point)))
          ((in-string-p point)
           nil)
          (t
           (let ((inside-indenting-paren nil)
                 (indent))
             (setf indent
                   (with-point ((point point))
                     (when (scan-lists point -1 1 t)
                       (case (character-at point)
                         (#\{
                          (back-to-indentation point)
                          (+ *go-tab-width* (point-column point)))
                         (#\(
                          (let ((n 0))
                            (when (with-point ((point point))
                                    (and (form-offset point -1)
                                         (member (following-word point)
                                                 '("import" "const" "var" "type" "package")
                                                 :test #'string=)))
                              (setf inside-indenting-paren t)
                              (incf n *go-tab-width*))
                            (back-to-indentation point)
                            (+ (point-column point) n)))))))
             (when (null indent)
               (return-from go-calc-indent 0))
             (cond ((looking-at point "case\\W|default\\W")
                    (decf indent *go-tab-width*))
                   ((with-point ((point point))
                      (when (looking-at (line-start point) "\\s*}")
                        (line-end point)
                        (skip-chars-backward point '(#\)))
                        (form-offset point -1)
                        (back-to-indentation point)
                        (setf indent (point-column point))))))
             (with-point ((point point))
               (line-start point)
               (skip-space-and-comment-backward point)
               (when (case (character-at point -1)
                       ((nil #\{ #\:)
                        nil)
                       (#\(
                        (not inside-indenting-paren))
                       (#\,
                        (and (scan-lists point -1 1 t)
                             (not (eql #\{ (character-at point)))))
                       (t
                        (not (semicolon-p point))))
                 (incf indent *go-tab-width*)))
             (with-point ((point point))
               (when (and (looking-at (line-start point) "^\\s*\\)\\s*$")
                          inside-indenting-paren)
                 (decf indent *go-tab-width*)))
             indent)))))

(define-key *go-mode-keymap* "}" 'go-electric-close)
(define-command go-electric-close (n) ("p")
  (self-insert n)
  (indent))

(define-command gofmt () ()
  (filter-buffer "gofmt"))

(pushnew (cons "\\.go$" 'go-mode) *auto-mode-alist* :test #'equal)
