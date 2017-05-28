(in-package :lem-lisp-mode)

(defun featurep (form)
  (cond ((atom form)
         (find (find-symbol (princ-to-string form)
                            :keyword)
               (features)))
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

(ppcre:define-parse-tree-synonym symbol
  (:greedy-repetition 1 nil
   (:inverted-char-class #\( #\) :whitespace-char-class #\; #\")))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    maybe-package-prefix
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor #\( #\))))

(defun make-tmlanguage-lisp ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region
                    `(:sequence ";")
                    "$"
                    :name 'syntax-comment-attribute)
                   (make-tm-region
                    `(:sequence "\"")
                    `(:sequence "\"")
                    :name 'syntax-string-attribute
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-region
                    `(:sequence "#|")
                    `(:sequence "|#")
                    :name 'syntax-comment-attribute)
                   (make-tm-match
                    "\\\\.")
                   (make-tm-match
                    `(:sequence
                      "("
                      (:sequence
                       ,(wrap-symbol-names
                         "defun" "defclass" "defgeneric"
                         "defsetf" "defmacro" "defmethod"))
                      (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "(" ,(wrap-symbol-names "defun")
                      ,(ppcre:parse-string "\\s*\\(")
                      ,(ppcre:parse-string "((?i:setf))\\s+") (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      (:group :case-insensitive-p
                       (:register (:sequence "define-" ,(ppcre:parse-string "\\S*"))))
                      (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor)
                      (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        "defvar" "defparameter" "defconstant")
                      (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        "deftype" "defpackage" "defstruct")
                      (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-type-attribute)))
                   (make-tm-match
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
                        "declare" "declaim" "proclaim"))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      symbol-boundary-begin
                      ":" symbol
                      symbol-boundary-end)
                    :name 'syntax-builtin-attribute)
                   (make-tm-match
                    `(:sequence
                      symbol-boundary-begin
                      "&" symbol
                      symbol-boundary-end)
                    :name 'syntax-constant-attribute)
                   (make-tm-match
                    "#[+-]"
                    :name 'syntax-comment-attribute
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
                                             (form-offset cur-point 1))))))))))))
    (make-tmlanguage :patterns patterns)))
