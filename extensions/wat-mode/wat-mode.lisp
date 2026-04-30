(defpackage :lem-wat-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:export :*wat-mode-hook*
           :*wat-mode-keymap*
           :*wat-syntax-table*
           :wat-mode))
(in-package :lem-wat-mode)

;;; WAT Keywords and Instructions

(defvar *wat-structure-keywords*
  '("module" "func" "param" "result" "local" "type"
    "import" "export" "memory" "data" "elem" "start"
    "global" "table" "mut" "offset"))

(defvar *wat-control-keywords*
  '("block" "loop" "if" "then" "else" "end"
    "br" "br_if" "br_table" "call" "call_indirect"
    "return" "unreachable" "nop"))

(defvar *wat-types*
  '("i32" "i64" "f32" "f64" "v128" "funcref" "externref" "anyfunc"))

(defvar *wat-instructions*
  '("local.get" "local.set" "local.tee"
    "global.get" "global.set"
    "i32.const" "i64.const" "f32.const" "f64.const"
    "i32.load" "i64.load" "f32.load" "f64.load"
    "i32.store" "i64.store" "f32.store" "f64.store"
    "i32.add" "i32.sub" "i32.mul" "i32.div_s" "i32.div_u"
    "i32.rem_s" "i32.rem_u" "i32.and" "i32.or" "i32.xor"
    "i32.shl" "i32.shr_s" "i32.shr_u" "i32.rotl" "i32.rotr"
    "i32.eq" "i32.ne" "i32.lt_s" "i32.lt_u" "i32.gt_s" "i32.gt_u"
    "i32.le_s" "i32.le_u" "i32.ge_s" "i32.ge_u" "i32.eqz"
    "i32.wrap_i64" "i32.trunc_f32_s" "i32.trunc_f64_s"
    "i64.add" "i64.sub" "i64.mul" "i64.div_s" "i64.div_u"
    "i64.extend_i32_s" "i64.extend_i32_u"
    "f32.add" "f32.sub" "f32.mul" "f32.div"
    "f32.neg" "f32.abs" "f32.sqrt" "f32.ceil" "f32.floor"
    "f64.add" "f64.sub" "f64.mul" "f64.div"
    "f64.neg" "f64.abs" "f64.sqrt" "f64.ceil" "f64.floor"
    "memory.size" "memory.grow" "memory.copy" "memory.fill"
    "drop" "select" "ref.null" "ref.func" "ref.is_null"
    "table.get" "table.set" "table.size" "table.grow"))

(defvar *wat-wast-keywords*
  '("assert_return" "assert_trap" "assert_exhaustion"
    "assert_malformed" "assert_invalid" "assert_unlinkable"
    "invoke" "get" "register"))

;;; TM Language Definition (fallback when tree-sitter not available)

(defun tokens (boundary strings)
  "Create a regex alternation pattern from STRINGS, optionally with word BOUNDARY."
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tmlanguage-wat ()
  (let* ((patterns
           (make-tm-patterns
            ;; Line comments ;;
            (make-tm-line-comment-region ";;")
            ;; Block comments (; ... ;)
            (make-tm-block-comment-region "(;" ";)")
            ;; Strings
            (make-tm-string-region "\"")
            ;; Numeric literals (hex, float, int)
            (make-tm-match "\\b0x[0-9a-fA-F]+\\b"
                           :name 'syntax-constant-attribute)
            (make-tm-match "\\b[+-]?[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?\\b"
                           :name 'syntax-constant-attribute)
            (make-tm-match "\\b[+-]?[0-9]+\\b"
                           :name 'syntax-constant-attribute)
            ;; Identifiers ($name)
            (make-tm-match "\\$[a-zA-Z0-9_.!#$%&'*+\\-/:<=>?@\\\\^`|~]+"
                           :name 'syntax-variable-attribute)
            ;; Types
            (make-tm-match (tokens :word-boundary *wat-types*)
                           :name 'syntax-type-attribute)
            ;; Structure keywords
            (make-tm-match (tokens :word-boundary *wat-structure-keywords*)
                           :name 'syntax-keyword-attribute)
            ;; Control flow keywords
            (make-tm-match (tokens :word-boundary *wat-control-keywords*)
                           :name 'syntax-keyword-attribute)
            ;; WAST keywords
            (make-tm-match (tokens :word-boundary *wat-wast-keywords*)
                           :name 'syntax-keyword-attribute)
            ;; Instructions
            (make-tm-match (tokens nil *wat-instructions*)
                           :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

;;; Syntax Table

(defvar *wat-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\)))
                :string-quote-chars '(#\")
                :line-comment-string ";;"
                :block-comment-pairs '(("(;" . ";)"))))
        (tmlanguage (make-tmlanguage-wat)))
    (set-syntax-parser table tmlanguage)
    table))

(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for WAT."
  (asdf:system-relative-pathname :lem-wat-mode "tree-sitter/highlights.scm"))

(defun try-enable-tree-sitter ()
  "Try to enable tree-sitter for WAT mode. Falls back to tmlanguage if unavailable."
  (ignore-errors
    (when (and (find-package :lem-tree-sitter)
               (fboundp (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter))
               (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter)))
      (funcall (find-symbol "ENABLE-TREE-SITTER-FOR-MODE" :lem-tree-sitter)
               *wat-syntax-table* "wat" (tree-sitter-query-path))
      t)))

(define-major-mode wat-mode language-mode
    (:name "WAT"
     :keymap *wat-mode-keymap*
     :syntax-table *wat-syntax-table*
     :mode-hook *wat-mode-hook*)
  ;; Try tree-sitter first, falls back to tmlanguage automatically
  (try-enable-tree-sitter)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) ";;"
        (variable-value 'insertion-line-comment) ";; "
        (variable-value 'calc-indent-function) 'lem-wat-mode/indent:calc-indent))

(define-file-type ("wat" "wast") wat-mode)
