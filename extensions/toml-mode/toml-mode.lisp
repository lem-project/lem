(defpackage :lem-toml-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*toml-mode-hook*
           :toml-mode))
(in-package :lem-toml-mode)

#| link: https://toml.io/en/v1.0.0 |#

(defun tokens (boundary strings)
  "Create a regex alternation pattern from STRINGS, optionally wrapped with BOUNDARY."
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-line-comment (separator)
  "Create a TextMate pattern for line comments starting with SEPARATOR."
  (make-tm-region separator "$" :name 'syntax-comment-attribute))

(defun make-tmlanguage-toml ()
  "Create a TextMate language definition for TOML syntax highlighting.
This serves as a fallback when tree-sitter is not available."
  (let* ((patterns (make-tm-patterns
                    ;; Comments
                    (make-tm-line-comment "#")
                    ;; Strings (basic and literal)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    ;; Multi-line strings
                    (make-tm-region "\"\"\"" "\"\"\"" :name 'syntax-string-attribute)
                    (make-tm-region "'''" "'''" :name 'syntax-string-attribute)
                    ;; Booleans
                    (make-tm-match (tokens :word-boundary '("true" "false"))
                                   :name 'syntax-keyword-attribute)
                    ;; Table headers [table] and [[array]]
                    (make-tm-match "^\\s*\\[\\[?[^\\]]+\\]\\]?"
                                   :name 'syntax-type-attribute)
                    ;; Punctuation and operators
                    (make-tm-match (tokens nil '("=" "," "[" "]" "{" "}" "."))
                                   :name 'syntax-builtin-attribute)
                    ;; Numbers (integers and floats)
                    (make-tm-match "\\b[+-]?[0-9][0-9_]*\\b"
                                   :name 'syntax-constant-attribute)
                    (make-tm-match "\\b[+-]?[0-9][0-9_]*\\.[0-9_]*\\b"
                                   :name 'syntax-constant-attribute)
                    ;; Special float values
                    (make-tm-match (tokens :word-boundary '("inf" "nan" "+inf" "-inf" "+nan" "-nan"))
                                   :name 'syntax-constant-attribute)
                    ;; Keys (bare keys at start of line or after newline)
                    (make-tm-match "^\\s*[a-zA-Z0-9_-]+"
                                   :name 'syntax-variable-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *toml-syntax-table*
  (let ((table (make-syntax-table
                :symbol-chars '(#\- #\_)
                :string-quote-chars '(#\" #\')
                :paren-pairs '((#\[ . #\])
                               (#\{ . #\}))))
        (tmlanguage (make-tmlanguage-toml)))
    (set-syntax-parser table tmlanguage)
    table)
  "Syntax table for TOML mode.")

(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for TOML."
  (asdf:system-relative-pathname :lem-toml-mode "tree-sitter/highlights.scm"))

(define-major-mode toml-mode language-mode
    (:name "Toml"
     :keymap *toml-mode-keymap*
     :syntax-table *toml-syntax-table*
     :mode-hook *toml-mode-hook*)
  "Major mode for editing TOML configuration files."
  (lem-tree-sitter:enable-tree-sitter-for-mode
   *toml-syntax-table* "toml" (tree-sitter-query-path))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "#"))

(define-file-type ("toml") toml-mode)
