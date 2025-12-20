(in-package :tree-sitter/types)

;;;; Language Class

(defclass ts-language ()
  ((ptr :initarg :ptr
        :reader ts-language-ptr
        :documentation "Pointer to TSLanguage")
   (name :initarg :name
         :reader ts-language-name
         :documentation "Language name (e.g., \"json\", \"c\")"))
  (:documentation "Wrapper for a tree-sitter language grammar."))

(defmethod print-object ((lang ts-language) stream)
  (print-unreadable-object (lang stream :type t)
    (format stream "~A" (ts-language-name lang))))

;;;; Parser Class

(defclass ts-parser ()
  ((ptr :initarg :ptr
        :reader ts-parser-ptr
        :documentation "Pointer to TSParser")
   (language :initarg :language
             :accessor ts-parser-language
             :initform nil
             :documentation "Associated language"))
  (:documentation "Wrapper for a tree-sitter parser."))

(defmethod initialize-instance :after ((parser ts-parser) &key)
  (let ((ptr (ts-parser-ptr parser)))
    (trivial-garbage:finalize parser
                              (lambda ()
                                (ffi:ts-parser-delete ptr)))))

(defmethod print-object ((parser ts-parser) stream)
  (print-unreadable-object (parser stream :type t)
    (if (ts-parser-language parser)
        (format stream "~A" (ts-language-name (ts-parser-language parser)))
        (format stream "no-language"))))

;;;; Tree Class

(defclass ts-tree ()
  ((ptr :initarg :ptr
        :reader ts-tree-ptr
        :documentation "Pointer to TSTree")
   (source :initarg :source
           :accessor ts-tree-source
           :initform nil
           :documentation "Original source string"))
  (:documentation "Wrapper for a tree-sitter syntax tree."))

(defmethod initialize-instance :after ((tree ts-tree) &key)
  (let ((ptr (ts-tree-ptr tree)))
    (trivial-garbage:finalize tree
                              (lambda ()
                                (ffi:ts-tree-delete ptr)))))

(defmethod print-object ((tree ts-tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)))

;;;; Node Class
;;; Nodes are transient - they reference a tree and contain position data

(defclass ts-node ()
  ((tree :initarg :tree
         :reader ts-node-tree
         :documentation "Parent tree")
   (buffer :initarg :buffer
           :reader ts-node-buffer
           :documentation "Foreign memory buffer for TSNode struct"))
  (:documentation "Wrapper for a tree-sitter node."))

(defmethod print-object ((node ts-node) stream)
  (print-unreadable-object (node stream :type t)
    (handler-case
        (format stream "~A [~D-~D]"
                (ffi:ts-node-type (ts-node-buffer node))
                (ffi:ts-node-start-byte (ts-node-buffer node))
                (ffi:ts-node-end-byte (ts-node-buffer node)))
      (error () (format stream "invalid")))))

;;;; Query Class

(defclass ts-query ()
  ((ptr :initarg :ptr
        :reader ts-query-ptr
        :documentation "Pointer to TSQuery")
   (language :initarg :language
             :reader ts-query-language
             :documentation "Language this query is for")
   (capture-names :initarg :capture-names
                  :reader ts-query-capture-names
                  :documentation "Vector of capture names"))
  (:documentation "Wrapper for a compiled tree-sitter query."))

(defmethod initialize-instance :after ((query ts-query) &key)
  (let ((ptr (ts-query-ptr query)))
    (trivial-garbage:finalize query
                              (lambda ()
                                (ffi:ts-query-delete ptr)))))

(defmethod print-object ((query ts-query) stream)
  (print-unreadable-object (query stream :type t)
    (format stream "~D patterns, ~D captures"
            (ffi:ts-query-pattern-count (ts-query-ptr query))
            (ffi:ts-query-capture-count (ts-query-ptr query)))))

;;;; Query Cursor Class

(defclass ts-query-cursor ()
  ((ptr :initarg :ptr
        :reader ts-query-cursor-ptr
        :documentation "Pointer to TSQueryCursor"))
  (:documentation "Cursor for iterating query matches."))

(defmethod initialize-instance :after ((cursor ts-query-cursor) &key)
  (let ((ptr (ts-query-cursor-ptr cursor)))
    (trivial-garbage:finalize cursor
                              (lambda ()
                                (ffi:ts-query-cursor-delete ptr)))))

;;;; Query Match Structure

(defstruct (query-match (:constructor make-query-match (pattern-index captures)))
  "Result of a query match."
  (pattern-index 0 :type fixnum :read-only t)
  (captures nil :type list :read-only t))

;;;; Query Capture Structure

(defstruct (query-capture (:constructor make-query-capture (node index name)))
  "A captured node from a query match."
  (node nil :read-only t)
  (index 0 :type fixnum :read-only t)
  (name "" :type string :read-only t))

;;;; Point Structure (for positions)

(defstruct (ts-point (:constructor make-ts-point (row column)))
  "A position in source code (0-indexed row and column)."
  (row 0 :type fixnum :read-only t)
  (column 0 :type fixnum :read-only t))

;;;; Input Edit Structure (for incremental parsing)

(defstruct (ts-input-edit
            (:constructor make-ts-input-edit
                (start-byte old-end-byte new-end-byte
                 start-point old-end-point new-end-point)))
  "Describes an edit to source code for incremental parsing."
  (start-byte 0 :type fixnum :read-only t)
  (old-end-byte 0 :type fixnum :read-only t)
  (new-end-byte 0 :type fixnum :read-only t)
  (start-point nil :type (or null ts-point) :read-only t)
  (old-end-point nil :type (or null ts-point) :read-only t)
  (new-end-point nil :type (or null ts-point) :read-only t))
