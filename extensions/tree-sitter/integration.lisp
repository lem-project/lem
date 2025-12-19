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
                 :documentation "Cached source for incremental parsing"))
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
  "Parse source text, reusing old tree if possible."
  (let ((old-tree (treesitter-parser-tree parser)))
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

(defun position-to-byte (buffer point)
  "Convert buffer position to byte offset.
   Assumes UTF-8 encoding."
  (let ((byte-offset 0)
        (target-line (lem:line-number-at-point point))
        (target-col (lem:point-charpos point)))
    (lem:with-point ((p (lem:buffer-start-point buffer)))
      (loop :for line-num :from 1
            :do (let* ((line (lem/buffer/internal::point-line p))
                       (str (lem/buffer/line:line-string line)))
                  (cond
                    ((< line-num target-line)
                     ;; Add full line + newline
                     (incf byte-offset
                           (+ (babel:string-size-in-octets str :encoding :utf-8)
                              1)))  ; newline
                    ((= line-num target-line)
                     ;; Add chars up to target column
                     (incf byte-offset
                           (babel:string-size-in-octets
                            (subseq str 0 (min target-col (length str)))
                            :encoding :utf-8))
                     (return byte-offset))
                    (t (return byte-offset))))
                (unless (lem:line-offset p 1)
                  (return byte-offset))))))

(defun byte-range-to-points (buffer start-byte end-byte)
  "Convert byte offsets to points. Returns (values start-point end-point)."
  (let ((start-point (byte-to-point buffer start-byte))
        (end-point (byte-to-point buffer end-byte)))
    (values start-point end-point)))

(defun byte-to-point (buffer byte-offset)
  "Convert byte offset to a point. Returns nil if out of range."
  (lem:with-point ((p (lem:buffer-start-point buffer)))
    (let ((current-byte 0))
      (loop :do (let* ((line (lem/buffer/internal::point-line p))
                       (str (lem/buffer/line:line-string line))
                       (line-bytes (babel:string-size-in-octets str :encoding :utf-8))
                       (line-end-byte (+ current-byte line-bytes 1)))  ; +1 for newline
                  (cond
                    ((<= current-byte byte-offset (1- line-end-byte))
                     ;; Found the line, now find column
                     (let ((col (byte-offset-to-char-offset str (- byte-offset current-byte))))
                       (lem:line-start p)
                       (lem:character-offset p col)
                       (return (lem:copy-point p :temporary))))
                    (t
                     (setf current-byte line-end-byte)
                     (unless (lem:line-offset p 1)
                       (return nil)))))))))

(defun byte-offset-to-char-offset (string byte-offset)
  "Convert a byte offset within a string to a character offset."
  (let ((bytes 0))
    (loop :for i :from 0 :below (length string)
          :for char := (char string i)
          :while (< bytes byte-offset)
          :do (incf bytes (babel:string-size-in-octets
                           (string char) :encoding :utf-8))
          :finally (return i))))

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

