(in-package :lem-tree-sitter/highlight)

;;;; Capture Name to Lem Attribute Mapping
;;;;
;;;; This module provides mapping from tree-sitter capture names to Lem
;;;; syntax highlighting attributes. Rather than using global mutable state,
;;;; we provide a constructor for creating capture-attribute maps that can
;;;; be passed explicitly to functions.

(defun make-default-capture-attribute-map ()
  "Create a new hash table mapping tree-sitter capture names to Lem attributes.
   Returns a fresh map populated with standard capture mappings following
   tree-sitter conventions used in nvim-treesitter and helix."
  (let ((map (make-hash-table :test 'equal)))
    (flet ((add (capture-name attribute)
             (setf (gethash capture-name map) attribute)))
      ;; Keywords and control flow
      (add "keyword" 'lem:syntax-keyword-attribute)
      (add "keyword.control" 'lem:syntax-keyword-attribute)
      (add "keyword.function" 'lem:syntax-keyword-attribute)
      (add "keyword.operator" 'lem:syntax-keyword-attribute)
      (add "keyword.return" 'lem:syntax-keyword-attribute)
      (add "keyword.conditional" 'lem:syntax-keyword-attribute)
      (add "keyword.repeat" 'lem:syntax-keyword-attribute)
      (add "keyword.import" 'lem:syntax-keyword-attribute)

      ;; Strings and literals
      (add "string" 'lem:syntax-string-attribute)
      (add "string.special" 'lem:syntax-string-attribute)
      (add "string.escape" 'lem:syntax-constant-attribute)
      (add "character" 'lem:syntax-string-attribute)

      ;; Numbers
      (add "number" 'lem:syntax-constant-attribute)
      (add "number.float" 'lem:syntax-constant-attribute)
      (add "float" 'lem:syntax-constant-attribute)

      ;; Comments
      (add "comment" 'lem:syntax-comment-attribute)
      (add "comment.line" 'lem:syntax-comment-attribute)
      (add "comment.block" 'lem:syntax-comment-attribute)
      (add "comment.documentation" 'lem:syntax-comment-attribute)

      ;; Functions
      (add "function" 'lem:syntax-function-name-attribute)
      (add "function.call" 'lem:syntax-function-name-attribute)
      (add "function.builtin" 'lem:syntax-builtin-attribute)
      (add "function.method" 'lem:syntax-function-name-attribute)
      (add "method" 'lem:syntax-function-name-attribute)

      ;; Types
      (add "type" 'lem:syntax-type-attribute)
      (add "type.builtin" 'lem:syntax-type-attribute)
      (add "type.definition" 'lem:syntax-type-attribute)

      ;; Variables and properties
      (add "variable" 'lem:syntax-variable-attribute)
      (add "variable.builtin" 'lem:syntax-builtin-attribute)
      (add "variable.parameter" 'lem:syntax-variable-attribute)
      (add "property" 'lem:syntax-variable-attribute)
      (add "field" 'lem:syntax-variable-attribute)

      ;; Constants
      (add "constant" 'lem:syntax-constant-attribute)
      (add "constant.builtin" 'lem:syntax-constant-attribute)
      (add "boolean" 'lem:syntax-constant-attribute)

      ;; Operators and punctuation
      (add "operator" 'lem:syntax-builtin-attribute)
      (add "punctuation" nil)  ; No highlight
      (add "punctuation.bracket" nil)
      (add "punctuation.delimiter" nil)

      ;; Labels and tags
      (add "label" 'lem:syntax-constant-attribute)
      (add "tag" 'lem:syntax-keyword-attribute)
      (add "attribute" 'lem:syntax-constant-attribute)

      ;; Errors
      (add "error" 'lem:compiler-note-attribute)

      ;;;; Markdown/Document-specific captures
      ;; Headers
      (add "markup.heading" 'lem:document-header1-attribute)
      (add "markup.heading.1" 'lem:document-header1-attribute)
      (add "markup.heading.2" 'lem:document-header2-attribute)
      (add "markup.heading.3" 'lem:document-header3-attribute)
      (add "markup.heading.4" 'lem:document-header4-attribute)
      (add "markup.heading.5" 'lem:document-header5-attribute)
      (add "markup.heading.6" 'lem:document-header6-attribute)
      ;; nvim-treesitter conventions
      (add "text.title" 'lem:document-header1-attribute)
      (add "text.title.1" 'lem:document-header1-attribute)
      (add "text.title.2" 'lem:document-header2-attribute)
      (add "text.title.3" 'lem:document-header3-attribute)
      (add "text.title.4" 'lem:document-header4-attribute)
      (add "text.title.5" 'lem:document-header5-attribute)
      (add "text.title.6" 'lem:document-header6-attribute)

      ;; Text formatting
      (add "markup.bold" 'lem:document-bold-attribute)
      (add "markup.italic" 'lem:document-italic-attribute)
      (add "markup.underline" 'lem:document-underline-attribute)
      (add "text.strong" 'lem:document-bold-attribute)
      (add "text.emphasis" 'lem:document-italic-attribute)

      ;; Code blocks and inline code
      (add "markup.raw" 'lem:document-code-block-attribute)
      (add "markup.raw.block" 'lem:document-code-block-attribute)
      (add "markup.raw.inline" 'lem:document-inline-code-attribute)
      (add "text.literal" 'lem:document-code-block-attribute)

      ;; Links and references
      (add "markup.link" 'lem:document-link-attribute)
      (add "markup.link.url" 'lem:document-link-attribute)
      (add "markup.link.text" 'lem:syntax-string-attribute)
      (add "text.uri" 'lem:document-link-attribute)
      (add "text.reference" 'lem:document-link-attribute)

      ;; Lists
      (add "markup.list" 'lem:document-list-attribute)
      (add "markup.list.checked" 'lem:document-task-list-attribute)
      (add "markup.list.unchecked" 'lem:document-task-list-attribute)

      ;; Block quotes
      (add "markup.quote" 'lem:document-blockquote-attribute)

      ;; Tables
      (add "markup.table" 'lem:document-table-attribute)

      ;; Metadata (frontmatter)
      (add "markup.meta" 'lem:document-metadata-attribute)

      ;; Punctuation special (for markdown markers)
      (add "punctuation.special" 'lem:syntax-builtin-attribute))
    map))

(defun default-capture-attribute-map ()
  "Return the default capture-attribute map.
   This map is created once at load time and reused."
  (load-time-value (make-default-capture-attribute-map) t))

(defun capture-to-attribute (capture-name &optional (capture-attribute-map (default-capture-attribute-map)))
  "Get the Lem attribute for CAPTURE-NAME using CAPTURE-ATTRIBUTE-MAP.
   If CAPTURE-ATTRIBUTE-MAP is not provided, uses the default map.
   Returns NIL if no mapping exists.
   Supports hierarchical lookup: 'keyword.control' falls back to 'keyword'."
  ;; Try exact match first
  (or (gethash capture-name capture-attribute-map)
      ;; Try base name (e.g., 'keyword.control' -> 'keyword')
      (let ((dot-pos (position #\. capture-name)))
        (when dot-pos
          (gethash (subseq capture-name 0 dot-pos) capture-attribute-map)))))

;;;; Query Loading

(defun load-highlight-query (language query-path)
  "Load a highlight query from a file path.
   LANGUAGE is the tree-sitter language name (e.g., \"json\").
   QUERY-PATH is the path to the highlights.scm file.
   Returns a compiled ts-query or NIL on error."
  (handler-case
      (let ((source (uiop:read-file-string query-path)))
        (ts:query-compile (ts:get-language language) source))
    (error (e)
      (log:warn "Failed to load highlight query ~A: ~A" query-path e)
      nil)))
