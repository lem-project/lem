(defpackage :lem-nix-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*nix-mode-hook*
           :*nix-mode-keymap*
           :*nix-syntax-table*
           :nix-mode))
(in-package :lem-nix-mode)

;;; Nix Keywords

(defvar *nix-keywords*
  '("let" "in" "if" "then" "else" "with" "rec" "inherit" "assert" "or"))

(defvar *nix-builtins*
  '("true" "false" "null"
    "builtins" "import" "throw" "abort" "derivation"
    "map" "filter" "foldl'" "toString" "toJSON" "fromJSON"
    "fetchurl" "fetchTarball" "fetchGit" "fetchTree"
    "baseNameOf" "dirOf" "isNull" "isBool" "isInt" "isFloat"
    "isString" "isList" "isAttrs" "isFunction" "isPath"
    "attrNames" "attrValues" "hasAttr" "getAttr" "removeAttrs"
    "listToAttrs" "catAttrs" "intersectAttrs"
    "head" "tail" "length" "elemAt" "concatLists" "concatMap"
    "elem" "all" "any" "genList" "sort" "partition"
    "add" "sub" "mul" "div" "lessThan" "ceil" "floor"
    "stringLength" "substring" "hashString" "match" "split"
    "replaceStrings" "concatStrings" "concatStringsSep"
    "seq" "deepSeq" "trace" "typeOf" "tryEval"
    "currentSystem" "currentTime" "nixVersion" "storeDir"))

;;; TM Language Definition (fallback when tree-sitter not available)

(defun tokens (boundary strings)
  "Create a regex alternation pattern from STRINGS, optionally with word BOUNDARY."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-nix ()
  "Create TextMate-style language definition for Nix."
  (let* ((patterns
           (make-tm-patterns
            ;; Line comments
            (make-tm-line-comment-region "#")
            ;; Block comments
            (make-tm-block-comment-region "/*" "*/")
            ;; Double-quoted strings
            (make-tm-string-region "\"")
            ;; Multi-line indented strings (basic support)
            (make-tm-region "''" "''"
                            :name 'syntax-string-attribute)
            ;; Numeric literals - floats
            (make-tm-match "\\b[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?\\b"
                           :name 'syntax-constant-attribute)
            ;; Numeric literals - integers
            (make-tm-match "\\b[0-9]+\\b"
                           :name 'syntax-constant-attribute)
            ;; Path literals - relative
            (make-tm-match "\\./[a-zA-Z0-9_./-]+"
                           :name 'syntax-string-attribute)
            ;; Path literals - absolute
            (make-tm-match "/[a-zA-Z][a-zA-Z0-9_./-]*"
                           :name 'syntax-string-attribute)
            ;; Path literals - home
            (make-tm-match "~/[a-zA-Z0-9_./-]+"
                           :name 'syntax-string-attribute)
            ;; Path literals - search path <nixpkgs>
            (make-tm-match "<[a-zA-Z0-9_./-]+>"
                           :name 'syntax-string-attribute)
            ;; Keywords
            (make-tm-match (tokens :word-boundary *nix-keywords*)
                           :name 'syntax-keyword-attribute)
            ;; Builtins
            (make-tm-match (tokens :word-boundary *nix-builtins*)
                           :name 'syntax-builtin-attribute)
            ;; Operators
            (make-tm-match (tokens nil '("//" "++" "->" "==" "!=" "<=" ">="
                                         "&&" "||" "?" "!" "@" ":"))
                           :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

;;; Syntax Table

(defvar *nix-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_ #\- #\')
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\")
                :line-comment-string "#"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (make-tmlanguage-nix)))
    (set-syntax-parser table tmlanguage)
    table))

;;; Tree-sitter Integration

(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for Nix."
  (asdf:system-relative-pathname :lem-nix-mode "tree-sitter/highlights.scm"))

(defun try-enable-tree-sitter ()
  "Try to enable tree-sitter for Nix mode. Falls back to tmlanguage if unavailable."
  (ignore-errors
    (when (and (find-package :lem-tree-sitter)
               (fboundp (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter))
               (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter)))
      (funcall (find-symbol "ENABLE-TREE-SITTER-FOR-MODE" :lem-tree-sitter)
               *nix-syntax-table* "nix" (tree-sitter-query-path))
      t)))

;;; Major Mode Definition

(define-major-mode nix-mode language-mode
    (:name "Nix"
     :keymap *nix-mode-keymap*
     :syntax-table *nix-syntax-table*
     :mode-hook *nix-mode-hook*)
  ;; Try tree-sitter first, falls back to tmlanguage automatically
  (try-enable-tree-sitter)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "#"
        (variable-value 'insertion-line-comment) "# "
        (variable-value 'calc-indent-function) 'lem-nix-mode/indent:calc-indent))

(define-file-type ("nix") nix-mode)
