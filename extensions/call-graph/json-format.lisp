(in-package #:call-graph)

;;; JSON Format for Call Graph
;;;
;;; This module provides bidirectional conversion between call-graph
;;; structures and JSON format. The JSON format serves as an intermediate
;;; representation for testing, debugging, and data exchange.
;;;
;;; JSON Schema:
;;; {
;;;   "nodes": [
;;;     {
;;;       "id": "string",
;;;       "name": "string",
;;;       "package": "string",
;;;       "type": "function|macro|generic-function|...",
;;;       "docstring": "string|null",
;;;       "arglist": "string|null",
;;;       "sourceFile": "string|null",
;;;       "sourceLine": number|null
;;;     }
;;;   ],
;;;   "edges": [
;;;     {
;;;       "source": "string",
;;;       "target": "string",
;;;       "callType": "direct|indirect"
;;;     }
;;;   ]
;;; }

;;; ============================================================
;;; call-graph → JSON (Task A-1)
;;; ============================================================

(defun graph-node-to-alist (node)
  "Convert a graph-node structure to an alist suitable for JSON encoding.

Returns an alist with the following keys:
  - \"id\": unique identifier
  - \"name\": symbol name
  - \"package\": package name
  - \"type\": node type as lowercase string
  - \"docstring\": documentation or null
  - \"arglist\": argument list or null
  - \"sourceFile\": source file path or null
  - \"sourceLine\": line number or null"
  (let ((source-loc (graph-node-source-location node)))
    (list (cons "id" (graph-node-id node))
          (cons "name" (graph-node-name node))
          (cons "package" (graph-node-package node))
          (cons "type" (string-downcase (symbol-name (graph-node-type node))))
          (cons "docstring" (graph-node-docstring node))
          (cons "arglist" (graph-node-arglist node))
          (cons "sourceFile" (graph-node-source-file node))
          (cons "sourceLine" (when source-loc (cdr source-loc))))))

(defun graph-edge-to-alist (edge)
  "Convert a graph-edge structure to an alist suitable for JSON encoding.

Returns an alist with the following keys:
  - \"source\": source node ID
  - \"target\": target node ID
  - \"callType\": type of call as lowercase string"
  (list (cons "source" (graph-edge-source edge))
        (cons "target" (graph-edge-target edge))
        (cons "callType" (string-downcase (symbol-name (graph-edge-call-type edge))))))

(defun call-graph-to-alist (graph)
  "Convert a call-graph structure to an alist suitable for JSON encoding.

Returns an alist with the following keys:
  - \"nodes\": array of node alists
  - \"edges\": array of edge alists"
  (let ((nodes '()))
    (maphash (lambda (id node)
               (declare (ignore id))
               (push (graph-node-to-alist node) nodes))
             (call-graph-nodes graph))
    (list (cons "nodes" (nreverse nodes))
          (cons "edges" (mapcar #'graph-edge-to-alist (call-graph-edges graph))))))

(defun call-graph-to-json (graph &optional (stream nil))
  "Convert a call-graph structure to a JSON string.

If STREAM is provided, write to it; otherwise return a string."
  (let ((alist (call-graph-to-alist graph)))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Encode nodes array
        (yason:with-object-element ("nodes")
          (yason:with-array ()
            (dolist (node-alist (cdr (assoc "nodes" alist :test #'string=)))
              (yason:with-object ()
                (loop :for (key . value) :in node-alist
                      :do (yason:encode-object-element key value))))))
        ;; Encode edges array
        (yason:with-object-element ("edges")
          (yason:with-array ()
            (dolist (edge-alist (cdr (assoc "edges" alist :test #'string=)))
              (yason:with-object ()
                (loop :for (key . value) :in edge-alist
                      :do (yason:encode-object-element key value))))))))))

;;; ============================================================
;;; JSON → call-graph (Task A-2)
;;; ============================================================

(defun alist-to-graph-node (alist)
  "Convert an alist (from JSON) to a graph-node structure.

ALIST should have keys: \"id\", \"name\", \"package\", \"type\",
optionally \"docstring\", \"arglist\", \"sourceFile\", \"sourceLine\"."
  (let ((source-file (cdr (assoc "sourceFile" alist :test #'string=)))
        (source-line (cdr (assoc "sourceLine" alist :test #'string=))))
    (make-graph-node
     :id (cdr (assoc "id" alist :test #'string=))
     :name (cdr (assoc "name" alist :test #'string=))
     :package (cdr (assoc "package" alist :test #'string=))
     :type (intern (string-upcase (cdr (assoc "type" alist :test #'string=))) :keyword)
     :docstring (cdr (assoc "docstring" alist :test #'string=))
     :arglist (cdr (assoc "arglist" alist :test #'string=))
     :source-file source-file
     :source-location (when (and source-file source-line)
                        (cons source-file source-line)))))

(defun alist-to-graph-edge (alist)
  "Convert an alist (from JSON) to a graph-edge structure.

ALIST should have keys: \"source\", \"target\", \"callType\"."
  (make-graph-edge
   :source (cdr (assoc "source" alist :test #'string=))
   :target (cdr (assoc "target" alist :test #'string=))
   :call-type (intern (string-upcase (cdr (assoc "callType" alist :test #'string=))) :keyword)))

(defun json-to-call-graph (json-string)
  "Parse a JSON string and convert it to a call-graph structure.

JSON-STRING should be a valid JSON object with \"nodes\" and \"edges\" arrays."
  (let* ((parsed (yason:parse json-string :object-as :alist))
         (nodes-alist (cdr (assoc "nodes" parsed :test #'string=)))
         (edges-alist (cdr (assoc "edges" parsed :test #'string=)))
         (graph (make-call-graph)))
    ;; Add nodes
    (dolist (node-alist nodes-alist)
      (add-node graph (alist-to-graph-node node-alist)))
    ;; Add edges
    (dolist (edge-alist edges-alist)
      (add-edge graph (alist-to-graph-edge edge-alist)))
    graph))
