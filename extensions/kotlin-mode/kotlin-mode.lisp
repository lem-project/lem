(defpackage :lem-kotlin-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*kotlin-mode-hook*
           :kotlin-mode))
(in-package :lem-kotlin-mode)

(defun tokens (boundary strings)
  "Create a regex pattern for matching a list of tokens."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-kotlin ()
  "Create TextMate language grammar for Kotlin syntax highlighting."
  (let* ((patterns (make-tm-patterns
                    ;; KDoc comment
                    (make-tm-region "/\\*\\*" "\\*/"
                                    :name 'syntax-comment-attribute)
                    ;; Block comment
                    (make-tm-region "/\\*" "\\*/"
                                    :name 'syntax-comment-attribute)
                    ;; Line comment
                    (make-tm-region "//" "$"
                                    :name 'syntax-comment-attribute)
                    ;; Raw string (triple-quoted)
                    (make-tm-region "\"\"\"" "\"\"\""
                                    :name 'syntax-string-attribute)
                    ;; String literals
                    (make-tm-region '(:sequence "\"")
                                    '(:sequence "\"")
                                    :name 'syntax-string-attribute
                                    :patterns (make-tm-patterns
                                               (make-tm-match "\\\\.")
                                               (make-tm-match "\\$\\{[^}]+\\}")
                                               (make-tm-match "\\$[a-zA-Z_][a-zA-Z0-9_]*")))
                    ;; Character literals
                    (make-tm-match "'\\\\?.'"
                                   :name 'syntax-string-attribute)
                    ;; Hard keywords
                    (make-tm-match (tokens :word-boundary
                                           '("as" "as?" "break" "class" "continue"
                                             "do" "else" "false" "for" "fun" "if"
                                             "in" "!in" "interface" "is" "!is"
                                             "null" "object" "package" "return"
                                             "super" "this" "throw" "true" "try"
                                             "typealias" "typeof" "val" "var"
                                             "when" "while"))
                                   :name 'syntax-keyword-attribute)
                    ;; Soft keywords
                    (make-tm-match (tokens :word-boundary
                                           '("by" "catch" "constructor" "delegate"
                                             "dynamic" "field" "file" "finally"
                                             "get" "import" "init" "param"
                                             "property" "receiver" "set" "setparam"
                                             "value" "where"))
                                   :name 'syntax-keyword-attribute)
                    ;; Modifier keywords
                    (make-tm-match (tokens :word-boundary
                                           '("abstract" "actual" "annotation"
                                             "companion" "const" "crossinline"
                                             "data" "enum" "expect" "external"
                                             "final" "infix" "inline" "inner"
                                             "internal" "lateinit" "noinline"
                                             "open" "operator" "out" "override"
                                             "private" "protected" "public"
                                             "reified" "sealed" "suspend"
                                             "tailrec" "vararg"))
                                   :name 'syntax-keyword-attribute)
                    ;; Annotations
                    (make-tm-match "@[a-zA-Z_][a-zA-Z0-9_]*"
                                   :name 'syntax-builtin-attribute)
                    ;; Function definitions
                    (make-tm-match `(:sequence
                                    (:register "fun")
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "([a-zA-Z_][a-zA-Z0-9_]*)")
                                    :word-boundary)
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-function-name-attribute)))
                    ;; Class/interface/object definitions
                    (make-tm-match `(:sequence
                                    (:register (:alternation "class" "interface" "object" "enum"))
                                    (:greedy-repetition 1 nil :whitespace-char-class)
                                    ,(ppcre:parse-string "([a-zA-Z_][a-zA-Z0-9_]*)")
                                    :word-boundary)
                                   :captures (vector nil
                                                     (make-tm-name 'syntax-keyword-attribute)
                                                     (make-tm-name 'syntax-type-attribute)))
                    ;; Primitive types
                    (make-tm-match (tokens :word-boundary
                                           '("Byte" "Short" "Int" "Long"
                                             "Float" "Double" "Boolean" "Char"
                                             "String" "Unit" "Nothing" "Any"))
                                   :name 'syntax-type-attribute)
                    ;; Collection types
                    (make-tm-match (tokens :word-boundary
                                           '("Array" "List" "Set" "Map"
                                             "MutableList" "MutableSet" "MutableMap"
                                             "ArrayList" "HashMap" "HashSet"
                                             "Pair" "Triple" "Sequence"))
                                   :name 'syntax-type-attribute)
                    ;; Type names (capitalized identifiers)
                    (make-tm-match "\\b[A-Z][a-zA-Z_0-9]*\\b"
                                   :name 'syntax-type-attribute)
                    ;; Constants
                    (make-tm-match (tokens :word-boundary
                                           '("null" "true" "false"))
                                   :name 'syntax-constant-attribute)
                    ;; Numeric literals (hex, binary, decimal, float)
                    (make-tm-match "\\b0[xX][0-9a-fA-F_]+[LuU]*\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b0[bB][01_]+[LuU]*\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[0-9][0-9_]*[LuU]*\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[0-9][0-9_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?[fF]?\\b"
                                   :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *kotlin-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\@)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\])
                               (#\< . #\>))
                :string-quote-chars '(#\" #\')
                :block-comment-pairs '(("/*" . "*/"))
                :line-comment-string "//")))
    (set-syntax-parser table (make-tmlanguage-kotlin))
    table)
  "Syntax table for Kotlin mode.")

;; Mode definition
(define-major-mode kotlin-mode language-mode
    (:name "Kotlin"
     :keymap *kotlin-mode-keymap*
     :syntax-table *kotlin-syntax-table*
     :mode-hook *kotlin-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'kotlin-calc-indent
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'beginning-of-defun-function) 'kotlin-beginning-of-defun
        (variable-value 'end-of-defun-function) 'kotlin-end-of-defun
        (variable-value 'line-comment) "//"
        (variable-value 'insertion-line-comment) "// "
        (variable-value 'tab-width :buffer) 4))

;; Key bindings
(define-key *kotlin-mode-keymap* "}" 'kotlin-electric-close)
(define-key *kotlin-mode-keymap* ")" 'kotlin-electric-close)
(define-key *kotlin-mode-keymap* "]" 'kotlin-electric-close)

;; Functions
(defun kotlin-beginning-of-defun (point n)
  "Move to the beginning of the current function definition."
  (loop :repeat n
        :do (search-backward-regexp point "^\\s*\\(\\(public\\|private\\|protected\\|internal\\|override\\|open\\|abstract\\|suspend\\|inline\\)\\s+\\)*fun\\s+")))

(defun kotlin-end-of-defun (point n)
  "Move to the end of the current function definition."
  (if (minusp n)
      (kotlin-beginning-of-defun point (- n))
      (search-forward-regexp point "^}")))

(defun kotlin-calc-indent (point)
  "Calculate the indentation for the current line in Kotlin mode."
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond
        ;; Inside string
        ((in-string-p p)
         nil)
        ;; Inside comment
        ((in-comment-p p)
         (with-point ((start p))
           (maybe-beginning-of-comment start)
           (if (eql #\* (character-at (back-to-indentation point)))
               (1+ (point-column start))
               (+ 2 (point-column start)))))
        ;; Closing brace/bracket/paren - match opening
        ((member (character-at p) '(#\} #\] #\)))
         (with-point ((tmp p))
           (when (scan-lists tmp -1 0 t)
             (back-to-indentation tmp)
             (point-column tmp))))
        (t
         ;; General case - look at previous lines
         (let ((indent 0))
           (with-point ((tmp p))
             (when (scan-lists tmp -1 1 t)
               (back-to-indentation tmp)
               (setf indent (+ tab-width (point-column tmp)))))
           ;; Adjust for else/catch/finally
           (with-point ((tmp p))
             (back-to-indentation tmp)
             (when (or (looking-at tmp "else\\b")
                       (looking-at tmp "catch\\b")
                       (looking-at tmp "finally\\b")
                       (looking-at tmp "->"))
               (when (> indent 0)
                 (decf indent tab-width))))
           indent))))))

;; Commands
(define-command kotlin-electric-close (n) (:universal)
  "Insert closing character and re-indent the current line."
  (self-insert n)
  (indent))

;; File type association
(define-file-type ("kt" "kts") kotlin-mode)
