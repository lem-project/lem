(in-package :lem-tree-sitter/highlight)

;;;; Capture Name to Lem Attribute Mapping

(defvar *capture-attribute-map* (make-hash-table :test 'equal)
  "Map from tree-sitter capture names to Lem attributes.")

(defun define-capture-mapping (capture-name attribute)
  "Define a mapping from CAPTURE-NAME to ATTRIBUTE."
  (setf (gethash capture-name *capture-attribute-map*) attribute))

(defun capture-to-attribute (capture-name)
  "Get the Lem attribute for a capture name.
   Returns NIL if no mapping exists."
  ;; Try exact match first
  (or (gethash capture-name *capture-attribute-map*)
      ;; Try base name (e.g., 'keyword.control' -> 'keyword')
      (let ((dot-pos (position #\. capture-name)))
        (when dot-pos
          (gethash (subseq capture-name 0 dot-pos) *capture-attribute-map*)))))

;;;; Standard Capture Mappings
;;; These follow tree-sitter conventions used in nvim-treesitter and helix

;; Keywords and control flow
(define-capture-mapping "keyword" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.control" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.function" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.operator" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.return" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.conditional" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.repeat" 'lem:syntax-keyword-attribute)
(define-capture-mapping "keyword.import" 'lem:syntax-keyword-attribute)

;; Strings and literals
(define-capture-mapping "string" 'lem:syntax-string-attribute)
(define-capture-mapping "string.special" 'lem:syntax-string-attribute)
(define-capture-mapping "string.escape" 'lem:syntax-constant-attribute)
(define-capture-mapping "character" 'lem:syntax-string-attribute)

;; Numbers
(define-capture-mapping "number" 'lem:syntax-constant-attribute)
(define-capture-mapping "number.float" 'lem:syntax-constant-attribute)
(define-capture-mapping "float" 'lem:syntax-constant-attribute)

;; Comments
(define-capture-mapping "comment" 'lem:syntax-comment-attribute)
(define-capture-mapping "comment.line" 'lem:syntax-comment-attribute)
(define-capture-mapping "comment.block" 'lem:syntax-comment-attribute)
(define-capture-mapping "comment.documentation" 'lem:syntax-comment-attribute)

;; Functions
(define-capture-mapping "function" 'lem:syntax-function-name-attribute)
(define-capture-mapping "function.call" 'lem:syntax-function-name-attribute)
(define-capture-mapping "function.builtin" 'lem:syntax-builtin-attribute)
(define-capture-mapping "function.method" 'lem:syntax-function-name-attribute)
(define-capture-mapping "method" 'lem:syntax-function-name-attribute)

;; Types
(define-capture-mapping "type" 'lem:syntax-type-attribute)
(define-capture-mapping "type.builtin" 'lem:syntax-type-attribute)
(define-capture-mapping "type.definition" 'lem:syntax-type-attribute)

;; Variables and properties
(define-capture-mapping "variable" 'lem:syntax-variable-attribute)
(define-capture-mapping "variable.builtin" 'lem:syntax-builtin-attribute)
(define-capture-mapping "variable.parameter" 'lem:syntax-variable-attribute)
(define-capture-mapping "property" 'lem:syntax-variable-attribute)
(define-capture-mapping "field" 'lem:syntax-variable-attribute)

;; Constants
(define-capture-mapping "constant" 'lem:syntax-constant-attribute)
(define-capture-mapping "constant.builtin" 'lem:syntax-constant-attribute)
(define-capture-mapping "boolean" 'lem:syntax-constant-attribute)

;; Operators and punctuation
(define-capture-mapping "operator" 'lem:syntax-builtin-attribute)
(define-capture-mapping "punctuation" nil)  ; No highlight
(define-capture-mapping "punctuation.bracket" nil)
(define-capture-mapping "punctuation.delimiter" nil)

;; Labels and tags
(define-capture-mapping "label" 'lem:syntax-constant-attribute)
(define-capture-mapping "tag" 'lem:syntax-keyword-attribute)
(define-capture-mapping "attribute" 'lem:syntax-constant-attribute)

;; Errors
(define-capture-mapping "error" 'lem:compiler-note-attribute)

;;;; Query Loading

(defun load-highlight-query (language query-path)
  "Load a highlight query from a file path.
   Returns a compiled ts-query or NIL on error."
  (handler-case
      (let ((source (uiop:read-file-string query-path)))
        (ts:query-compile (ts:get-language language) source))
    (error (e)
      (lem:message "Failed to load highlight query ~A: ~A" query-path e)
      nil)))
