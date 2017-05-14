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

(ppcre:define-parse-tree-synonym symbol-boundary-begin
  (:alternation
   :start-anchor
   (:positive-lookbehind (:char-class #\( #\) :whitespace-char-class))))

(ppcre:define-parse-tree-synonym symbol-boundary-end
  (:alternation
   :end-anchor
   (:positive-lookahead  (:char-class #\( #\) :whitespace-char-class))))

(ppcre:define-parse-tree-synonym maybe-package-prefix
  (:greedy-repetition 0 1
   (:sequence
    (:greedy-repetition 1 nil (:inverted-char-class #\( #\) #\space #\tab)) #\:)))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    maybe-package-prefix
    (:register
     (:group :case-insensitive-p
      (:alternation
       ,@(apply #'word-length-sort names))))
    (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor #\( #\))))

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
    (add-syntax-pattern
     table
     (make-syntax-region
      (make-regex-matcher `(:sequence ";"))
      (make-regex-matcher "$")
      :attribute 'syntax-comment-attribute))
    (add-syntax-pattern
     table
     (make-syntax-region
      (make-regex-matcher `(:sequence "\""))
      (make-regex-matcher `(:sequence "\""))
      :attribute 'syntax-string-attribute
      :patterns (make-syntax-patterns
                 (make-syntax-match (make-regex-matcher "\\\\.")))))
    (add-syntax-pattern
     table
     (make-syntax-region
      (make-regex-matcher `(:sequence "#|"))
      (make-regex-matcher `(:sequence "|#"))
      :attribute 'syntax-comment-attribute))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher "\\\\.")))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher
       `(:sequence
         "("
         (:sequence
          ,(wrap-symbol-names
            "defun" "defclass" "defgeneric"
            "defsetf" "defmacro" "defmethod"))
         (:register ,(ppcre:parse-string "[^() \\t]+"))))
      :captures (vector nil
                        (make-syntax-name :attribute 'syntax-keyword-attribute)
                        (make-syntax-name :attribute 'syntax-function-name-attribute))))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher
       `(:sequence
         "("
         (:group :case-insensitive-p
          (:register (:sequence "define-" ,(ppcre:parse-string "\\S*"))))
         (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor)
         (:register ,(ppcre:parse-string "[^() \\t]+"))))
      :captures (vector nil
                        (make-syntax-name :attribute 'syntax-keyword-attribute)
                        (make-syntax-name :attribute 'syntax-function-name-attribute))))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher
       `(:sequence
         "("
         ,(wrap-symbol-names
           "defvar" "defparameter" "defconstant")
         (:register ,(ppcre:parse-string "[^() \\t]+"))))
      :captures (vector nil
                        (make-syntax-name :attribute 'syntax-keyword-attribute)
                        (make-syntax-name :attribute 'syntax-variable-attribute))))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher
       `(:sequence
         "("
         ,(wrap-symbol-names
           "deftype" "defpackage" "defstruct")
         (:register ,(ppcre:parse-string "[^() \\t]+"))))
      :captures (vector nil
                        (make-syntax-name :attribute 'syntax-keyword-attribute)
                        (make-syntax-name :attribute 'syntax-type-attribute))))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher
       `(:sequence
         "("
         ,(wrap-symbol-names
           "block" "case" "ccase" "ecase" "typecase" "etypecase" "ctypecase" "catch"
           "cond" "destructuring-bind" "do" "do*" "dolist" "dotimes"
           "eval-when" "flet" "labels" "macrolet" "generic-flet" "generic-labels"
           "handler-case" "restart-case" "if" "lambda" "let" "let*" "handler-bind"
           "restart-bind" "locally" "multiple-value-bind" "multiple-value-call"
           "multiple-value-prog1" "prog" "prog*" "prog1" "prog2" "progn" "progv" "return"
           "return-from" "symbol-macrolet" "tagbody" "throw" "unless" "unwind-protect"
           "when" "with-accessors" "with-condition-restarts" "with-open-file"
           "with-output-to-string" "with-slots" "with-standard-io-syntax" "loop"
           "declare" "declaim" "proclaim")))
      :captures (vector nil
                        (make-syntax-name :attribute 'syntax-keyword-attribute))))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher `(:sequence
                            symbol-boundary-begin
                            ,(ppcre:parse-string ":[^()\" \\t]+")
                            symbol-boundary-end))
      :attribute 'syntax-constant-attribute))
    (add-syntax-pattern
     table
     (make-syntax-match
      (make-regex-matcher `(:sequence
                            symbol-boundary-begin
                            ,(ppcre:parse-string "&[^() \\t]+")
                            symbol-boundary-end))
      :attribute 'syntax-constant-attribute))
    ;; (syntax-add-match
    ;;  table
    ;;  (make-regex-matcher "#[+-]")
    ;;  :move-action (lambda (cur-point)
    ;;                 (ignore-errors
    ;;                  (let ((positivep (eql #\+ (character-at cur-point 1))))
    ;;                    (character-offset cur-point 2)
    ;;                    (with-point ((prev cur-point))
    ;;                      (when (form-offset cur-point 1)
    ;;                        (cond
    ;;                          ((if (featurep (read-from-string
    ;;                                          (points-to-string
    ;;                                           prev cur-point)))
    ;;                               positivep
    ;;                               (not positivep))
    ;;                           nil)
    ;;                          (t
    ;;                           (form-offset cur-point 1))))))))
    ;;  :attribute 'syntax-comment-attribute)
    table))
