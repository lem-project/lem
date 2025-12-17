(in-package #:call-graph)

;;; Data Structures
;;;
;;; These structures represent a language-agnostic call graph.
;;; They can be populated by any provider (Common Lisp, LSP, etc.)

(defparameter *unknown-source-id* "file:(unknown source)"
  "Sentinel ID for grouping functions whose source file cannot be determined.")

(defstruct graph-node
  "Represents a function/symbol node in the call graph.

Slots:
  id            - Unique identifier (typically \"PACKAGE:NAME\")
  name          - Symbol/function name
  package       - Package/module name
  type          - Node type (:function, :macro, :generic-function, :command, etc.)
  docstring     - Documentation string
  arglist       - Formatted argument list string
  source-location - (pathname . line-number) cons
  source-file   - Full path to source file
  position      - Reserved for future use"
  (id "" :type string)
  (name "" :type string)
  (package "" :type string)
  (type :function :type keyword)
  (docstring nil :type (or null string))
  (arglist nil :type (or null string))
  (source-location nil :type (or null cons))
  (source-file nil :type (or null string))
  (position nil :type (or null cons)))

(defstruct graph-edge
  "Represents a call relationship between functions.

Slots:
  source    - Caller node ID
  target    - Callee node ID
  call-type - Type of call (:direct, :indirect, etc.)"
  (source "" :type string)
  (target "" :type string)
  (call-type :direct :type keyword))

(defstruct call-graph
  "Complete call graph containing nodes and edges.

Slots:
  nodes        - Hash table mapping node ID to graph-node
  edges        - List of graph-edge
  root-package - Optional root package object"
  (nodes (make-hash-table :test 'equal) :type hash-table)
  (edges nil :type list)
  (root-package nil))

;;; Utility Functions

(defun make-node-id (package-name symbol-name)
  "Create a unique node ID from package and symbol names."
  (format nil "~A:~A" package-name symbol-name))

(defun add-node (graph node)
  "Add a node to the call graph. Returns the node."
  (setf (gethash (graph-node-id node) (call-graph-nodes graph)) node)
  node)

(defun add-edge (graph edge)
  "Add an edge to the call graph. Returns the edge."
  (push edge (call-graph-edges graph))
  edge)

(defun find-node (graph node-id)
  "Find a node by ID in the call graph."
  (gethash node-id (call-graph-nodes graph)))

(defun remove-duplicate-edges (edges)
  "Remove duplicate edges (same source and target)."
  (remove-duplicates edges
                     :test (lambda (a b)
                             (and (string= (graph-edge-source a)
                                           (graph-edge-source b))
                                  (string= (graph-edge-target a)
                                           (graph-edge-target b))))))

;;; JSON Serialization for Cytoscape.js

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table for JSON encoding."
  (let ((ht (make-hash-table :test 'equal)))
    (loop :for (key . value) :in alist
          :do (setf (gethash key ht)
                    (if (and (consp value) (consp (car value)))
                        (alist-to-hash-table value)
                        value)))
    ht))

(defun make-file-node-id (filepath)
  "Create a unique ID for a file node."
  (format nil "file:~A" (or filepath "unknown")))

(defun short-filename (filepath)
  "Extract just the filename from a path."
  (if filepath
      (file-namestring filepath)
      "unknown"))

(defun graph-to-cytoscape-json (graph &key (encoder #'identity))
  "Convert a call-graph to Cytoscape.js compatible JSON string.

ENCODER should be a function that takes a hash-table and returns a JSON string.
If not provided, returns the data structure for the caller to encode."
  (let ((elements '())
        (files (make-hash-table :test 'equal))
        (has-orphans nil))
    ;; First pass: collect unique source files and check for orphans
    (maphash (lambda (id node)
               (declare (ignore id))
               (let ((source-file (graph-node-source-file node)))
                 (if source-file
                     (setf (gethash source-file files) t)
                     (setf has-orphans t))))
             (call-graph-nodes graph))
    ;; Add file parent nodes
    (maphash (lambda (filepath _)
               (declare (ignore _))
               (push (alist-to-hash-table
                      (list (cons "group" "nodes")
                            (cons "data"
                                  (list (cons "id" (make-file-node-id filepath))
                                        (cons "name" (short-filename filepath))
                                        (cons "type" "file")
                                        (cons "filepath" filepath)))))
                     elements))
             files)
    ;; Add unknown source container if there are orphan nodes
    (when has-orphans
      (push (alist-to-hash-table
             (list (cons "group" "nodes")
                   (cons "data"
                         (list (cons "id" *unknown-source-id*)
                               (cons "name" "(unknown source)")
                               (cons "type" "file")
                               (cons "filepath" "")))))
            elements))
    ;; Add function nodes with parent reference
    (maphash (lambda (id node)
               (declare (ignore id))
               (let* ((source-file (graph-node-source-file node))
                      (source-loc (graph-node-source-location node))
                      (line-number (when source-loc (cdr source-loc)))
                      (parent-id (if source-file
                                      (make-file-node-id source-file)
                                      *unknown-source-id*)))
                 (push (alist-to-hash-table
                        (list (cons "group" "nodes")
                              (cons "data"
                                    (list (cons "id" (graph-node-id node))
                                          (cons "name" (graph-node-name node))
                                          (cons "type"
                                                (string-downcase
                                                 (symbol-name (graph-node-type node))))
                                          (cons "package" (graph-node-package node))
                                          (cons "docstring"
                                                (or (graph-node-docstring node) ""))
                                          (cons "arglist"
                                                (or (graph-node-arglist node) ""))
                                          (cons "sourceFile"
                                                (or (short-filename source-file) ""))
                                          (cons "lineNumber" (or line-number 0))
                                          (cons "parent" parent-id)))))
                       elements)))
             (call-graph-nodes graph))
    ;; Add edges
    (loop :for edge :in (call-graph-edges graph)
          :for i :from 0
          :do (push (alist-to-hash-table
                     (list (cons "group" "edges")
                           (cons "data"
                                 (list (cons "id" (format nil "edge-~D" i))
                                       (cons "source" (graph-edge-source edge))
                                       (cons "target" (graph-edge-target edge))))))
                    elements))
    ;; Return data structure
    (let ((result (alexandria:plist-hash-table
                   (list "elements" (coerce (nreverse elements) 'vector))
                   :test 'equal)))
      (funcall encoder result))))
