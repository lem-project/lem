(in-package :lem-tree-sitter)

;;;; Tree-sitter Parser for Lem

(defclass treesitter-parser (lem/buffer/internal::syntax-parser)
  ((language-name :initarg :language-name
                  :reader treesitter-parser-language-name
                  :documentation "Language name (e.g., \"json\")")
   (parser :initarg :parser
           :accessor treesitter-parser-handle
           :documentation "Tree-sitter parser instance")
   (tree :initform nil
         :accessor treesitter-parser-tree
         :documentation "Current syntax tree")
   (highlight-query :initarg :highlight-query
                    :accessor treesitter-parser-highlight-query
                    :initform nil
                    :documentation "Compiled highlight query")
   (source-cache :initform nil
                 :accessor treesitter-parser-source-cache
                 :documentation "Cached source for incremental parsing")
   (pending-edits :initform nil
                  :accessor treesitter-parser-pending-edits
                  :documentation "List of pending edits for incremental parsing"))
  (:documentation "Tree-sitter based syntax parser for Lem."))

(defun make-treesitter-parser (language-name &key highlight-query-path)
  "Create a treesitter-parser for the given language."
  (let* ((language (ts:get-language language-name))
         (parser (ts:make-parser language))
         (query (when highlight-query-path
                  (highlight:load-highlight-query language-name highlight-query-path))))
    (make-instance 'treesitter-parser
                   :language-name language-name
                   :parser parser
                   :highlight-query query)))

;;;; Core Integration: %syntax-scan-region

(defmethod lem/buffer/internal::%syntax-scan-region ((parser treesitter-parser) start end)
  "Scan region using tree-sitter and apply syntax highlighting."
  (let* ((buffer (lem:point-buffer start))
         (source (get-buffer-text buffer)))
    ;; Clear old attributes in the region
    (clear-attributes-in-region start end)
    ;; Parse the buffer
    (let ((tree (parse-buffer-text parser source)))
      (when tree
        ;; Apply highlighting
        (apply-tree-highlights parser tree buffer start end)))))

(defun clear-attributes-in-region (start end)
  "Clear syntax highlighting attributes in the region from START to END."
  (lem:with-point ((p start))
    (loop :while (lem:point<= p end)
          :do (let ((line (lem/buffer/internal::point-line p)))
                (lem/buffer/line:line-clear-property line :attribute))
              (unless (lem:line-offset p 1)
                (return)))))

(defun get-buffer-text (buffer)
  "Get the full text of a buffer as a string."
  (lem:buffer-text buffer))

(defun parse-buffer-text (parser source)
  "Parse source text, using incremental parsing when possible."
  (let ((old-tree (treesitter-parser-tree parser))
        (pending-edits (treesitter-parser-pending-edits parser)))
    ;; Apply pending edits to old tree for incremental parsing
    (when (and old-tree pending-edits)
      (dolist (edit (nreverse pending-edits))
        (ts:tree-edit old-tree edit)))
    ;; Clear pending edits
    (setf (treesitter-parser-pending-edits parser) nil)
    ;; Parse (tree-sitter will reuse unchanged parts when old-tree has edits applied)
    (setf (treesitter-parser-source-cache parser) source)
    (let ((new-tree (ts:parser-parse-string (treesitter-parser-handle parser)
                                            source
                                            old-tree)))
      (setf (treesitter-parser-tree parser) new-tree)
      new-tree)))

;;;; Highlighting Application

(defun apply-tree-highlights (parser tree buffer start end)
  "Apply syntax highlights from tree-sitter to buffer region."
  (let ((query (treesitter-parser-highlight-query parser)))
    (when query
      (let* ((root (ts:tree-root-node tree))
             (start-byte (position-to-byte buffer start))
             (end-byte (position-to-byte buffer end))
             (captures (ts:query-captures-in-range query root start-byte end-byte)))
        (dolist (capture captures)
          (apply-capture-highlight buffer capture))))))

(defun apply-capture-highlight (buffer capture)
  "Apply highlighting for a single capture."
  (let* ((node (ts:capture-node capture))
         (capture-name (ts:capture-name capture))
         (attribute (highlight:capture-to-attribute capture-name)))
    (when attribute
      (let ((start-byte (ts:node-start-byte node))
            (end-byte (ts:node-end-byte node)))
        (apply-attribute-to-byte-range buffer start-byte end-byte attribute)))))

(defun apply-attribute-to-byte-range (buffer start-byte end-byte attribute)
  "Apply attribute to a byte range in the buffer."
  (multiple-value-bind (start-point end-point)
      (byte-range-to-points buffer start-byte end-byte)
    (when (and start-point end-point)
      (apply-attribute-between-points start-point end-point attribute))))

(defun apply-attribute-between-points (start end attribute)
  "Apply attribute between two points, handling multi-line spans."
  (lem:with-point ((p start))
    (loop :while (lem:point< p end)
          :do (let* ((line (lem/buffer/internal::point-line p))
                     (line-end (lem:line-end (lem:copy-point p :temporary)))
                     (col-start (lem:point-charpos p))
                     (col-end (if (lem:point<= line-end end)
                                  (lem/buffer/line:line-length line)
                                  (lem:point-charpos end))))
                (when (< col-start col-end)
                  (lem/buffer/line:line-add-property line col-start col-end
                                                     :attribute attribute nil))
                (unless (lem:line-offset p 1)
                  (return))
                (lem:line-start p)))))

;;;; Position Conversion Utilities
;;;; Using Lem's built-in byte offset functions from src/buffer/internal/basic.lisp

(defun position-to-byte (buffer point)
  "Convert buffer position to byte offset using Lem's point-bytes."
  (declare (ignore buffer))
  (lem:point-bytes point))

(defun byte-range-to-points (buffer start-byte end-byte)
  "Convert byte offsets to points. Returns (values start-point end-point)."
  (let ((start-point (byte-to-point buffer start-byte))
        (end-point (byte-to-point buffer end-byte)))
    (values start-point end-point)))

(defun byte-to-point (buffer byte-offset)
  "Convert byte offset to a point using Lem's move-to-bytes.
   Note: move-to-bytes has an off-by-one behavior for byte-offset > 0,
   so we advance until we reach the correct byte position."
  (lem:with-point ((p (lem:buffer-start-point buffer)))
    (lem:move-to-bytes p byte-offset)
    ;; Advance until we reach the correct byte position
    (loop :while (and (< (lem:point-bytes p) byte-offset)
                      (not (lem:end-buffer-p p)))
          :do (lem:character-offset p 1))
    (lem:copy-point p :temporary)))

;;;; Utility Functions

(defun tree-sitter-available-p ()
  "Check if tree-sitter is available."
  (ts:tree-sitter-available-p))

(defun query-path-for (language)
  "Get the path to the highlight query for LANGUAGE."
  (asdf:system-relative-pathname
   :lem-tree-sitter (format nil "queries/~A/highlights.scm" language)))

;;;; Enable tree-sitter for existing modes

(defun enable-tree-sitter-for-mode (syntax-table language)
  "Enable tree-sitter syntax highlighting for a mode's syntax table.
   SYNTAX-TABLE: The mode's syntax table to update
   LANGUAGE: tree-sitter language name (e.g., \"json\")"
  (when (tree-sitter-available-p)
    (let ((query-path (query-path-for language)))
      (when (probe-file query-path)
        (handler-case
            (progn
              (unless (ts:get-language language)
                (ts:load-language-from-system language))
              (let ((parser (make-treesitter-parser
                             language
                             :highlight-query-path query-path)))
                (lem:set-syntax-parser syntax-table parser)
                t))
          (error ()
            ;; Silently fail - tree-sitter is optional
            nil))))))

;;;; Incremental Parsing Support

(defun get-buffer-treesitter-parser (buffer)
  "Get the treesitter-parser for BUFFER, if any."
  (let* ((syntax-table (lem:buffer-syntax-table buffer))
         (parser (when syntax-table
                   (lem:syntax-table-parser syntax-table))))
    (when (typep parser 'treesitter-parser)
      parser)))

(defun point-to-ts-point (point)
  "Convert a Lem point to a tree-sitter point (0-indexed row, byte column)."
  (let* ((row (1- (lem:line-number-at-point point)))  ; 0-indexed
         (line-start (lem:copy-point point :temporary)))
    (lem:line-start line-start)
    (let ((column-bytes (- (lem:point-bytes point)
                           (lem:point-bytes line-start))))
      (ts:make-ts-point row column-bytes))))

(defun record-tree-sitter-edit (start end old-len)
  "Record an edit for incremental tree-sitter parsing.
   START: point at start of changed region
   END: point at end of changed region (after change)
   OLD-LEN: length of old text that was replaced"
  (let* ((buffer (lem:point-buffer start))
         (parser (get-buffer-treesitter-parser buffer)))
    (when parser
      (let* ((start-byte (lem:point-bytes start))
             (new-end-byte (lem:point-bytes end))
             ;; old-end-byte = start-byte + old-len (in bytes)
             ;; We need to calculate bytes from old-len (chars)
             ;; Since old text is already deleted, we approximate using the difference
             (old-end-byte (+ start-byte old-len))
             (start-point (point-to-ts-point start))
             (new-end-point (point-to-ts-point end))
             ;; For old-end-point, we approximate since old text is gone
             ;; This is a limitation - we set it same as start for deletions
             (old-end-point (if (zerop old-len)
                                start-point
                                ;; Approximate: assume single line edit
                                (ts:make-ts-point
                                 (ts:ts-point-row start-point)
                                 (+ (ts:ts-point-column start-point) old-len)))))
        (push (ts:make-ts-input-edit start-byte old-end-byte new-end-byte
                                     start-point old-end-point new-end-point)
              (treesitter-parser-pending-edits parser))))))

;; Register global hook for incremental parsing
;; This checks if buffer has a treesitter-parser and records edits accordingly
(lem:add-hook (lem:variable-value 'lem:after-change-functions :global)
              'record-tree-sitter-edit)
