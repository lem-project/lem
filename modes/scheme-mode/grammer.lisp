(in-package :lem-scheme-mode)

(ppcre:define-parse-tree-synonym symbol
  (:alternation
   (:sequence
    #\|
    (:greedy-repetition 0 nil
     (:alternation
      (:group "\\|")
      (:inverted-char-class #\|)))
    #\|)
   (:greedy-repetition 1 nil
    (:inverted-char-class #\( #\) :whitespace-char-class #\; #\"))))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation
     (:greedy-repetition 1 nil :whitespace-char-class)
     :whitespace-char-class :end-anchor #\( #\))))

(defun make-tmlanguage-scheme ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region
                    `(:sequence ";")
                    "$"
                    :name 'syntax-comment-attribute)
                   ;; todo: nested comment
                   (make-tm-region
                    `(:sequence "#|")
                    `(:sequence "|#")
                    :name 'syntax-comment-attribute)
                   (make-tm-region
                    `(:sequence "|")
                    `(:sequence "|")
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-region
                    `(:sequence "\"")
                    `(:sequence "\"")
                    :name 'syntax-string-attribute
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   ;; shebang (for Gauche)
                   (make-tm-region
                    `(:sequence :start-anchor "#!")
                    "$"
                    :name 'syntax-comment-attribute)
                   ;; regexp (for Gauche)
                   (make-tm-region
                    `(:sequence "#/")
                    `(:sequence "/" (:greedy-repetition 0 1 #\i))
                    :name 'syntax-constant-attribute
                    :patterns (make-tm-patterns
                               (make-tm-match "\\\\.")))
                   (make-tm-match
                    "\\\\.")
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define")
                      "("
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define-method" "define-generic")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define-class" "define-record-type")
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-type-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      (:group
                       (:register (:sequence "define-" ,(ppcre:parse-string "\\S*"))))
                      (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor)
                      (:greedy-repetition 0 1 (:register symbol)))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      (:group
                       (:register (:sequence "make-" ,(ppcre:parse-string "\\S*"))))
                      (:alternation (:greedy-repetition 1 nil :whitespace-char-class) :end-anchor))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        ;; added
                        "use" "import" "cond" "cond-expand" "set!"
                        "eq?" "eqv?" "equal?" "and" "or" "not"
                        ;; part 1
                        "begin" "case" "delay" "do" "export" "if" "lambda"
                        "let" "let*" "letrec" "letrec*" "library"
                        "syntax-case" "with-input-from-file"
                        "with-output-to-file"
                        ;; part 2
                        "and-let1" "ecase" "glet1" "hash-table" "if-let1"
                        "match-lambda" "match-lambda*" "match-let"
                        "match-let*" "match-letrec" "match-let1"
                        "rlet1" "unwind-protect"
                        ;; part 3
                        "and-let*" "begin0" "call-with-client-socket"
                        "call-with-input-conversion" "call-with-input-file"
                        "call-with-input-process" "call-with-input-string"
                        "call-with-iterator" "call-with-output-conversion"
                        "call-with-output-file" "call-with-output-string"
                        "call-with-temporary-file" "call-with-values"
                        "dolist" "dotimes" "if-match" "let*-values"
                        "let-args" "let-keywords*" "let-match"
                        "let-optionals*" "let-syntax" "let-values"
                        "let/cc" "let1" "letrec-syntax" "make"
                        "multiple-value-bind" "match" "parameterize"
                        "parse-options" "receive" "rxmatch-case"
                        "rxmatch-cond" "rxmatch-if" "rxmatch-let"
                        "syntax-rules" "unless" "until" "when"
                        "while" "with-builder" "with-error-handler"
                        "with-error-to-port" "with-input-conversion"
                        "with-input-from-port" "with-input-from-process"
                        "with-input-from-string" "with-iterator"
                        "with-module" "with-output-conversion"
                        "with-output-to-port" "with-output-to-process"
                        "with-output-to-string" "with-port-locking"
                        "with-string-io" "with-time-counter"
                        "with-signal-handlers" "with-locking-mutex"
                        "guard"))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    `(:sequence
                      "(" ,(wrap-symbol-names "error" "errorf"))
                    :captures (vector nil (make-tm-name 'syntax-warning-attribute)))
                   )))
    (make-tmlanguage :patterns patterns)))
