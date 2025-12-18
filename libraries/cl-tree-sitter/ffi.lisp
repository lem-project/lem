(in-package :cl-tree-sitter/ffi)

;;;; Library Definition

(cffi:define-foreign-library tree-sitter
  (:unix (:or "libtree-sitter.so.0" "libtree-sitter.so"))
  (:darwin (:or "libtree-sitter.0.dylib" "libtree-sitter.dylib"))
  (t (:default "tree-sitter")))

;; Wrapper library for by-value struct handling
(cffi:define-foreign-library ts-wrapper
  (:unix "libts-wrapper.so")
  (:darwin "libts-wrapper.dylib")
  (t (:default "ts-wrapper")))

(defvar *tree-sitter-loaded* nil)
(defvar *ts-wrapper-loaded* nil)

(defun reset-library-state ()
  "Reset library loading state. Called at image startup."
  (setf *tree-sitter-loaded* nil
        *ts-wrapper-loaded* nil))

;; Reset state when image is restored (for Nix builds)
#+sbcl
(pushnew 'reset-library-state sb-ext:*init-hooks*)

(defun ensure-tree-sitter-loaded ()
  (unless *tree-sitter-loaded*
    (handler-case
        (progn
          (cffi:load-foreign-library 'tree-sitter)
          (setf *tree-sitter-loaded* t))
      (error ()
        nil))))

(defun ensure-ts-wrapper-loaded ()
  "Try to load the wrapper library for by-value struct handling."
  (unless *ts-wrapper-loaded*
    (handler-case
        (progn
          (cffi:load-foreign-library 'ts-wrapper)
          (setf *ts-wrapper-loaded* t))
      (error ()
        nil))))

(defun tree-sitter-available-p ()
  "Check if tree-sitter library is available."
  (ensure-tree-sitter-loaded)
  *tree-sitter-loaded*)

;;;; Basic Type Definitions

;; TSPoint - 8 bytes (2 x uint32)
(cffi:defcstruct ts-point
  (row :uint32)
  (column :uint32))

(defun ts-point-row (point)
  (cffi:foreign-slot-value point '(:struct ts-point) 'row))

(defun ts-point-column (point)
  (cffi:foreign-slot-value point '(:struct ts-point) 'column))

;; TSInputEdit - 32 bytes
(cffi:defcstruct ts-input-edit
  (start-byte :uint32)
  (old-end-byte :uint32)
  (new-end-byte :uint32)
  (start-point (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

;; TSNode - 24 bytes (opaque, passed by value in C API)
;; We use a wrapper approach with tree cursors for traversal
(cffi:defcstruct ts-node-raw
  (context :uint32 :count 4)
  (id :pointer)
  (tree :pointer))

;; TSTreeCursor - for node traversal without by-value issues
(cffi:defcstruct ts-tree-cursor
  (tree :pointer)
  (id :pointer)
  (context :uint32 :count 3))

;; TSQueryMatch
(cffi:defcstruct ts-query-match
  (id :uint32)
  (pattern-index :uint16)
  (capture-count :uint16)
  (captures :pointer))

;; TSQueryCapture
(cffi:defcstruct ts-query-capture
  (node (:struct ts-node-raw))
  (index :uint32))

;;;; Parser Functions

(cffi:defcfun ("ts_parser_new" ts-parser-new) :pointer
  "Create a new parser.")

(cffi:defcfun ("ts_parser_delete" ts-parser-delete) :void
  "Delete a parser."
  (parser :pointer))

(cffi:defcfun ("ts_parser_set_language" ts-parser-set-language) :bool
  "Set the language for a parser."
  (parser :pointer)
  (language :pointer))

(cffi:defcfun ("ts_parser_language" ts-parser-language) :pointer
  "Get the parser's current language."
  (parser :pointer))

(cffi:defcfun ("ts_parser_parse_string" %ts-parser-parse-string) :pointer
  "Parse a string and return a tree."
  (parser :pointer)
  (old-tree :pointer)
  (string :string)
  (length :uint32))

(defun ts-parser-parse-string (parser old-tree string)
  "Parse a string, handling length automatically."
  (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
    (%ts-parser-parse-string parser old-tree string (length bytes))))

(cffi:defcfun ("ts_parser_reset" ts-parser-reset) :void
  "Reset the parser state."
  (parser :pointer))

(cffi:defcfun ("ts_parser_set_timeout_micros" ts-parser-set-timeout-micros) :void
  "Set parser timeout in microseconds."
  (parser :pointer)
  (timeout :uint64))

;;;; Tree Functions

(cffi:defcfun ("ts_tree_delete" ts-tree-delete) :void
  "Delete a syntax tree."
  (tree :pointer))

(cffi:defcfun ("ts_tree_copy" ts-tree-copy) :pointer
  "Create a shallow copy of a tree."
  (tree :pointer))

(cffi:defcfun ("ts_tree_language" ts-tree-language) :pointer
  "Get the language used to parse the tree."
  (tree :pointer))

;; ts_tree_root_node returns TSNode by value - use our wrapper
(cffi:defcfun ("ts_tree_root_node_out" %ts-tree-root-node-out) :void
  "Get the root node of a tree (wrapper with out-pointer)."
  (out-node :pointer)
  (tree :pointer))

(defun ts-tree-root-node (tree node-buffer)
  "Get the root node of a tree, storing it in NODE-BUFFER.
   Returns NODE-BUFFER for convenience."
  (ensure-ts-wrapper-loaded)
  (%ts-tree-root-node-out node-buffer tree)
  node-buffer)

(cffi:defcfun ("ts_tree_edit" ts-tree-edit) :void
  "Edit a tree for incremental parsing."
  (tree :pointer)
  (edit :pointer))

;;;; Node Functions (using wrapper functions that take pointer to node)

(cffi:defcfun ("ts_node_type_ptr" %ts-node-type-ptr) :string
  (node :pointer))

(defun ts-node-type (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-type-ptr node))

(cffi:defcfun ("ts_node_symbol_ptr" %ts-node-symbol-ptr) :uint16
  (node :pointer))

(defun ts-node-symbol (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-symbol-ptr node))

(cffi:defcfun ("ts_node_start_byte_ptr" %ts-node-start-byte-ptr) :uint32
  (node :pointer))

(defun ts-node-start-byte (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-start-byte-ptr node))

(cffi:defcfun ("ts_node_end_byte_ptr" %ts-node-end-byte-ptr) :uint32
  (node :pointer))

(defun ts-node-end-byte (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-end-byte-ptr node))

;; Point functions - use wrapper
(cffi:defcfun ("ts_node_start_point_ptr" %ts-node-start-point-ptr) :void
  (out-point :pointer)
  (node :pointer))

(defun ts-node-start-point (node point-buffer)
  (ensure-ts-wrapper-loaded)
  (%ts-node-start-point-ptr point-buffer node)
  point-buffer)

(cffi:defcfun ("ts_node_end_point_ptr" %ts-node-end-point-ptr) :void
  (out-point :pointer)
  (node :pointer))

(defun ts-node-end-point (node point-buffer)
  (ensure-ts-wrapper-loaded)
  (%ts-node-end-point-ptr point-buffer node)
  point-buffer)

(cffi:defcfun ("ts_node_child_count_ptr" %ts-node-child-count-ptr) :uint32
  (node :pointer))

(defun ts-node-child-count (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-child-count-ptr node))

(cffi:defcfun ("ts_node_named_child_count_ptr" %ts-node-named-child-count-ptr) :uint32
  (node :pointer))

(defun ts-node-named-child-count (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-named-child-count-ptr node))

;; Child access - use wrapper
(cffi:defcfun ("ts_node_child_ptr" %ts-node-child-ptr) :void
  (out-node :pointer)
  (node :pointer)
  (index :uint32))

(defun ts-node-child (node index out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-child-ptr out-node node index)
  out-node)

(cffi:defcfun ("ts_node_named_child_ptr" %ts-node-named-child-ptr) :void
  (out-node :pointer)
  (node :pointer)
  (index :uint32))

(defun ts-node-named-child (node index out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-named-child-ptr out-node node index)
  out-node)

;; Parent/sibling - use wrapper
(cffi:defcfun ("ts_node_parent_ptr" %ts-node-parent-ptr) :void
  (out-node :pointer)
  (node :pointer))

(defun ts-node-parent (node out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-parent-ptr out-node node)
  out-node)

(cffi:defcfun ("ts_node_next_sibling_ptr" %ts-node-next-sibling-ptr) :void
  (out-node :pointer)
  (node :pointer))

(defun ts-node-next-sibling (node out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-next-sibling-ptr out-node node)
  out-node)

(cffi:defcfun ("ts_node_prev_sibling_ptr" %ts-node-prev-sibling-ptr) :void
  (out-node :pointer)
  (node :pointer))

(defun ts-node-prev-sibling (node out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-prev-sibling-ptr out-node node)
  out-node)

(cffi:defcfun ("ts_node_next_named_sibling_ptr" %ts-node-next-named-sibling-ptr) :void
  (out-node :pointer)
  (node :pointer))

(defun ts-node-next-named-sibling (node out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-next-named-sibling-ptr out-node node)
  out-node)

(cffi:defcfun ("ts_node_prev_named_sibling_ptr" %ts-node-prev-named-sibling-ptr) :void
  (out-node :pointer)
  (node :pointer))

(defun ts-node-prev-named-sibling (node out-node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-prev-named-sibling-ptr out-node node)
  out-node)

;; Node predicates - use wrapper
(cffi:defcfun ("ts_node_is_null_ptr" %ts-node-is-null-ptr) :bool
  (node :pointer))

(defun ts-node-is-null (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-is-null-ptr node))

(cffi:defcfun ("ts_node_is_named_ptr" %ts-node-is-named-ptr) :bool
  (node :pointer))

(defun ts-node-is-named (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-is-named-ptr node))

(cffi:defcfun ("ts_node_is_missing_ptr" %ts-node-is-missing-ptr) :bool
  (node :pointer))

(defun ts-node-is-missing (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-is-missing-ptr node))

(cffi:defcfun ("ts_node_is_extra_ptr" %ts-node-is-extra-ptr) :bool
  (node :pointer))

(defun ts-node-is-extra (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-is-extra-ptr node))

(cffi:defcfun ("ts_node_has_error_ptr" %ts-node-has-error-ptr) :bool
  (node :pointer))

(defun ts-node-has-error (node)
  (ensure-ts-wrapper-loaded)
  (%ts-node-has-error-ptr node))

(cffi:defcfun ("ts_node_string_ptr" %ts-node-string-ptr) :pointer
  (node :pointer))

(defun ts-node-string (node)
  "Get S-expression string representation of node. Caller must free result."
  (ensure-ts-wrapper-loaded)
  (let ((ptr (%ts-node-string-ptr node)))
    (unless (cffi:null-pointer-p ptr)
      (prog1 (cffi:foreign-string-to-lisp ptr)
        (cffi:foreign-free ptr)))))

;;;; Tree Cursor Functions

(cffi:defcfun ("ts_tree_cursor_new" %ts-tree-cursor-new) :void
  (out-cursor :pointer)
  (node :pointer))

(defun ts-tree-cursor-new (node cursor-buffer)
  (%ts-tree-cursor-new cursor-buffer node)
  cursor-buffer)

(cffi:defcfun ("ts_tree_cursor_delete" ts-tree-cursor-delete) :void
  (cursor :pointer))

(cffi:defcfun ("ts_tree_cursor_reset" %ts-tree-cursor-reset) :void
  (cursor :pointer)
  (node :pointer))

(defun ts-tree-cursor-reset (cursor node)
  (%ts-tree-cursor-reset cursor node))

(cffi:defcfun ("ts_tree_cursor_current_node" %ts-tree-cursor-current-node) :void
  (out-node :pointer)
  (cursor :pointer))

(defun ts-tree-cursor-current-node (cursor node-buffer)
  (%ts-tree-cursor-current-node node-buffer cursor)
  node-buffer)

(cffi:defcfun ("ts_tree_cursor_current_field_name" ts-tree-cursor-current-field-name) :string
  (cursor :pointer))

(cffi:defcfun ("ts_tree_cursor_goto_parent" ts-tree-cursor-goto-parent) :bool
  (cursor :pointer))

(cffi:defcfun ("ts_tree_cursor_goto_next_sibling" ts-tree-cursor-goto-next-sibling) :bool
  (cursor :pointer))

(cffi:defcfun ("ts_tree_cursor_goto_first_child" ts-tree-cursor-goto-first-child) :bool
  (cursor :pointer))

;;;; Query Functions

(cffi:defcfun ("ts_query_new" %ts-query-new) :pointer
  (language :pointer)
  (source :string)
  (source-len :uint32)
  (error-offset :pointer)
  (error-type :pointer))

(defun ts-query-new (language source)
  "Create a new query. Returns (query error-offset error-type)."
  (ensure-tree-sitter-loaded)
  (cffi:with-foreign-objects ((error-offset :uint32)
                              (error-type :uint32))
    (let* ((source-bytes (babel:string-to-octets source :encoding :utf-8))
           (query (%ts-query-new language source (length source-bytes)
                                 error-offset error-type)))
      (if (cffi:null-pointer-p query)
          (values nil
                  (cffi:mem-ref error-offset :uint32)
                  (cffi:mem-ref error-type :uint32))
          (values query nil nil)))))

(cffi:defcfun ("ts_query_delete" ts-query-delete) :void
  (query :pointer))

(cffi:defcfun ("ts_query_pattern_count" ts-query-pattern-count) :uint32
  (query :pointer))

(cffi:defcfun ("ts_query_capture_count" ts-query-capture-count) :uint32
  (query :pointer))

(cffi:defcfun ("ts_query_capture_name_for_id" %ts-query-capture-name-for-id) :pointer
  (query :pointer)
  (id :uint32)
  (length :pointer))

(defun ts-query-capture-name-for-id (query id)
  "Get capture name for an ID."
  (cffi:with-foreign-object (length :uint32)
    (let ((ptr (%ts-query-capture-name-for-id query id length)))
      (unless (cffi:null-pointer-p ptr)
        (cffi:foreign-string-to-lisp ptr :count (cffi:mem-ref length :uint32))))))

;;;; Query Cursor Functions

(cffi:defcfun ("ts_query_cursor_new" ts-query-cursor-new) :pointer
  "Create a new query cursor.")

(cffi:defcfun ("ts_query_cursor_delete" ts-query-cursor-delete) :void
  (cursor :pointer))

;; Original ts_query_cursor_exec takes TSNode by value, so we use our wrapper
(cffi:defcfun ("ts_query_cursor_exec_ptr" %ts-query-cursor-exec-ptr) :void
  (cursor :pointer)
  (query :pointer)
  (node :pointer))

(defun ts-query-cursor-exec (cursor query node)
  "Execute a query on a node using a cursor. NODE should be a node buffer pointer."
  (ensure-ts-wrapper-loaded)
  (%ts-query-cursor-exec-ptr cursor query node))

(cffi:defcfun ("ts_query_cursor_next_match" ts-query-cursor-next-match) :bool
  (cursor :pointer)
  (match :pointer))

(cffi:defcfun ("ts_query_cursor_next_capture" ts-query-cursor-next-capture) :bool
  (cursor :pointer)
  (match :pointer)
  (capture-index :pointer))

(cffi:defcfun ("ts_query_cursor_set_byte_range" ts-query-cursor-set-byte-range) :void
  (cursor :pointer)
  (start-byte :uint32)
  (end-byte :uint32))

(cffi:defcfun ("ts_query_cursor_set_point_range" ts-query-cursor-set-point-range) :void
  (cursor :pointer)
  (start-point :pointer)
  (end-point :pointer))

;;;; Language Functions

(cffi:defcfun ("ts_language_version" ts-language-version) :uint32
  (language :pointer))

(cffi:defcfun ("ts_language_symbol_count" ts-language-symbol-count) :uint32
  (language :pointer))

(cffi:defcfun ("ts_language_symbol_name" ts-language-symbol-name) :string
  (language :pointer)
  (symbol :uint16))
