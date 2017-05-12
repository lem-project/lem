(defpackage :lem-lisp-syntax.syntax-table
  (:use :cl :lem-base)
  (:export :*get-features-function*
           :*syntax-table*))
(in-package :lem-lisp-syntax.syntax-table)

(defvar *get-features-function* nil)

(flet ((f (c1 c2 step-fn)
         (when c1
           (when (and (member c1 '(#\#))
                      (or (alphanumericp c2)
                          (member c2 '(#\+ #\-))))
             (funcall step-fn)))))
  
  (defun skip-expr-prefix-forward (point)
    (f (character-at point 0)
       (character-at point 1)
       (lambda ()
         (character-offset point 2))))
  
  (defun skip-expr-prefix-backward (point)
    (f (character-at point -2)
       (character-at point -1)
       (lambda ()
         (character-offset point -2)))))

(defun featurep (form)
  (cond ((atom form)
         (find (find-symbol (princ-to-string form)
                            :keyword)
               (if *get-features-function*
                   (funcall *get-features-function*)
                   *features*)))
        ((string-equal 'and (car form))
         (every #'featurep (cdr form)))
        ((string-equal 'or (car form))
         (some #'featurep (cdr form)))
        (t)))

(defparameter +symbol-package-prefix+
  '(:sequence
    (:greedy-repetition 1 nil (:inverted-char-class #\( #\) #\space #\tab)) #\:))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(ppcre:define-parse-tree-synonym symbol-boundary-begin
  (:alternation
   :start-anchor
   (:positive-lookbehind (:char-class #\( #\) :whitespace-char-class))))

(ppcre:define-parse-tree-synonym symbol-boundary-end
  (:alternation
   :end-anchor
   (:positive-lookahead  (:char-class #\( #\) :whitespace-char-class))))

(defvar *syntax-table*
  (let ((table
         (make-syntax-table
          :space-chars '(#\space #\tab #\newline)
          :symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@ #\[ #\]
                          #\^ #\{ #\} #\~ #\# #\|)
          :paren-alist '((#\( . #\))
                         (#\[ . #\])
                         (#\{ . #\}))
          :string-quote-chars '(#\")
          :escape-chars '(#\\)
          :fence-chars '(#\|)
          :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
          :expr-prefix-forward-function 'skip-expr-prefix-forward
          :expr-prefix-backward-function 'skip-expr-prefix-backward
          :line-comment-string ";"
          :block-comment-pairs '(("#|" . "|#")))))
    (syntax-add-match table
                      (make-regexp-matcher `(:sequence
                                             symbol-boundary-begin
                                             ,(ppcre:parse-string ":[^()\" \\t]+")
                                             symbol-boundary-end))
                      :attribute 'syntax-constant-attribute)
    (syntax-add-match table
                      (make-regexp-matcher "\\(")
                      :matched-symbol :start-form)
    (syntax-add-match table
                      (make-regexp-matcher "[^() \\t]+")
                      :test-symbol :define-start
                      :attribute 'syntax-function-name-attribute)
    (syntax-add-match table
                      (make-regexp-matcher "[^() \\t]+")
                      :test-symbol :defpackage-start
                      :attribute 'syntax-type-attribute)
    (syntax-add-match table
                      (make-regexp-matcher "[^() \\t]+")
                      :test-symbol :defvar-start
                      :attribute 'syntax-variable-attribute)
    (syntax-add-match table
                      (make-regexp-matcher
                       `(:sequence
                         symbol-boundary-begin
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "defun" "defclass" "defgeneric" "defsetf" "defmacro" "defmethod")
                          (:sequence "define-" (:greedy-repetition 0 nil
                                                (:inverted-char-class #\space #\tab #\( #\)))))
                         symbol-boundary-end))
                      :test-symbol :start-form
                      :attribute 'syntax-keyword-attribute
                      :matched-symbol :define-start)
    (syntax-add-match table
                      (make-regexp-matcher
                       `(:sequence
                         symbol-boundary-begin
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "deftype" "defpackage" "defstruct"))
                         symbol-boundary-end))
                      :test-symbol :start-form
                      :attribute 'syntax-keyword-attribute
                      :matched-symbol :defpackage-start)
    (syntax-add-match table
                      (make-regexp-matcher
                       `(:sequence
                         symbol-boundary-begin
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort "defvar" "defparameter" "defconstant"))
                         symbol-boundary-end))
                      :test-symbol :start-form
                      :attribute 'syntax-keyword-attribute
                      :matched-symbol :defvar-start)
    (syntax-add-match table
                      (make-regexp-matcher
                       `(:sequence
                         symbol-boundary-begin
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "block" "case" "ccase" "ecase" "typecase" "etypecase" "ctypecase" "catch"
                             "cond" "destructuring-bind" "do" "do*" "dolist" "dotimes"
                             "eval-when" "flet" "labels" "macrolet" "generic-flet" "generic-labels"
                             "handler-case" "restart-case" "if" "lambda" "let" "let*" "handler-bind"
                             "restart-bind" "locally" "multiple-value-bind" "multiple-value-call"
                             "multiple-value-prog1" "prog" "prog*" "prog1" "prog2" "progn" "progv" "return"
                             "return-from" "symbol-macrolet" "tagbody" "throw" "unless" "unwind-protect"
                             "when" "with-accessors" "with-condition-restarts" "with-open-file"
                             "with-output-to-string" "with-slots" "with-standard-io-syntax" "loop"
                             "declare" "declaim" "proclaim"))
                         symbol-boundary-end))
                      :test-symbol :start-form
                      :attribute 'syntax-keyword-attribute)
    (syntax-add-match table
                      (make-regexp-matcher `(:sequence
                                             symbol-boundary-begin
                                             ,(ppcre:parse-string "&[^() \\t]+")
                                             symbol-boundary-end))
                      :attribute 'syntax-constant-attribute)
    (syntax-add-match
     table
     (make-regexp-matcher "#[+-]")
     :move-action (lambda (cur-point)
                    (ignore-errors
                     (let ((positivep (eql #\+ (character-at cur-point 1))))
                       (character-offset cur-point 2)
                       (with-point ((prev cur-point))
                         (when (form-offset cur-point 1)
                           (cond
                             ((if (featurep (read-from-string
                                             (points-to-string
                                              prev cur-point)))
                                  positivep
                                  (not positivep))
                              nil)
                             (t
                              (form-offset cur-point 1))))))))
     :attribute 'syntax-comment-attribute)
    table))
