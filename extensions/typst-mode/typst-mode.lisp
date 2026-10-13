(defpackage :lem-typst-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*typst-mode-hook*
   :*typst-mode-keymap*
           :*typst-syntax-table*
   :typst-mode))
(in-package :lem-typst-mode)

(defparameter *typst-keywords*
  '("let" "set" "show" "import" "include" "return"
    "if" "else" "while" "for" "in" "break" "continue"
    "not" "and" "or" "as" "context"))

(defparameter *typst-constants*
  '("none" "auto" "true" "false"))

(defparameter *typst-builtins*
  '("align" "block" "box" "circle" "cite" "counter" "datetime"
    "document" "ellipse" "enum" "figure" "footnote" "grid"
    "heading" "image" "layout" "line" "link" "list" "locate"
    "lorem" "metadata" "move" "page" "pagebreak" "panic"
    "par" "parbreak" "path" "place" "polygon" "query" "raw"
    "rect" "regex" "repeat" "rotate" "scale" "square" "state"
    "str" "table" "text" "type"))

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-typst ()
  (let* ((patterns
           (make-tm-patterns
            ;; Comments  
            (make-tm-line-comment-region "//")
            (make-tm-block-comment-region "/*" "*/")

            ;; Titles (= h1, == h2, etc.)
            (make-tm-match "^\\s*=+\\s+.*$"
                           :name 'lem:document-header1-attribute)

            ;; Raw blocks (`code` et ```code```)
            (make-tm-region "```" "```"
                            :name 'lem:document-code-block-attribute)
            (make-tm-region "`" "`"
                            :name 'lem:document-inline-code-attribute)

            ;; math mode ($...$) 
            (make-tm-region "\\$" "\\$"
                            :name 'syntax-constant-attribute)

            ;; ("text")
            (make-tm-match "\\\\\"")
            (make-tm-string-region "\"")

            ;; (@ref and <label>)
            (make-tm-match "@[a-zA-Z0-9_-]+"
                           :name 'syntax-variable-attribute)
            (make-tm-match "<[a-zA-Z0-9_-]+>"
                           :name 'syntax-variable-attribute)

            ;; (integer, floats, length: 10pt, 2em, 50%)
            (make-tm-match "\\b[0-9]+(\\.[0-9]+)?(pt|mm|cm|in|em|deg|rad)?\\b|\\b[0-9]+(\\.[0-9]+)?%"
                           :name 'syntax-constant-attribute)

            ;; (#let, #set, let, if, etc.)
            (make-tm-match (tokens :word-boundary *typst-keywords*)
                           :name 'syntax-keyword-attribute)
            (make-tm-match (tokens :word-boundary *typst-constants*)
                           :name 'syntax-constant-attribute)

            ;; function
            (make-tm-match (tokens :word-boundary *typst-builtins*)
                           :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *typst-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\- #\# #\$ #\@)
                :paren-pairs '((#\( . #\))
                               (#\[ . #\])
                               (#\{ . #\}))
                :string-quote-chars '(#\" #\`)
                :escape-chars '(#\\)
                :expr-prefix-chars '(#\# #\$)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-typst)))
    (set-syntax-parser table tmlanguage)
    table)
  "Syntax table for Typst mode.")

(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for Typst."
  (asdf:system-relative-pathname :lem-typst-mode "tree-sitter/highlights.scm"))

(defun tree-sitter-indent-path ()
  "Return the path to the tree-sitter indent query for Typst."
  (asdf:system-relative-pathname :lem-typst-mode "tree-sitter/indents.scm"))

(defun typst-typstyle (buf)
  "Format file in place using typstyle."
  (let ((file (buffer-filename buf)))
    (when file
      (uiop:run-program
       (list "typstyle" "-i" (namestring file))
       :ignore-error-status t)
      (revert-buffer t))))

(define-major-mode typst-mode language-mode
  (:name "Typst"
   :keymap *typst-mode-keymap*
   :syntax-table *typst-syntax-table*
   :mode-hook *typst-mode-hook*
   :formatter #'typst-typstyle)
  (let ((query-path (tree-sitter-query-path))
        (indent-path (tree-sitter-indent-path)))
    (when (and query-path (probe-file query-path))
      (lem-tree-sitter:enable-tree-sitter-for-mode
       *typst-syntax-table*
       "typst"
       query-path
       :indent-query-path (when (and indent-path (probe-file indent-path))
                            indent-path))))
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "//"
        (variable-value 'insertion-line-comment) "// "))

(define-file-type ("typ" "typst") typst-mode)
