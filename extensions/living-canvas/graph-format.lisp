(defpackage :lem-living-canvas/graph-format
  (:use :cl)
  (:export #:convert-to-cytoscape
           #:parse-living-canvas-json
           #:validate-living-canvas-json-p))
(in-package :lem-living-canvas/graph-format)

;;; Living Canvas JSON to Cytoscape.js Conversion
;;;
;;; This module converts the portable Living Canvas JSON format
;;; to Cytoscape.js-specific format for rendering.

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table for YASON encoding."
  (let ((ht (make-hash-table :test 'equal)))
    (loop :for (key . value) :in alist
          :do (setf (gethash key ht)
                    (if (and (consp value) (consp (car value)))
                        (alist-to-hash-table value)
                        value)))
    ht))

(defun convert-node-to-cytoscape (node)
  "Convert a Living Canvas node to Cytoscape.js node format."
  (let* ((id (gethash "id" node))
         (name (gethash "name" node))
         (package (gethash "package" node))
         (type (gethash "type" node))
         (attributes (gethash "attributes" node))
         (layout (gethash "layout" node))
         (arglist (when attributes (gethash "arglist" attributes)))
         (docstring (when attributes (gethash "docstring" attributes)))
         (source (when attributes (gethash "source" attributes)))
         (source-file (when source (gethash "file" source)))
         (line-number (when source (gethash "line" source)))
         (position (when layout (gethash "position" layout)))
         (parent-id (gethash "parent" node))
         (data-alist (list (cons "id" id)
                           (cons "name" name)
                           (cons "type" type)
                           (cons "package" (or package ""))
                           (cons "docstring" (or docstring ""))
                           (cons "arglist" (or arglist ""))
                           (cons "sourceFile" (or (and source-file (file-namestring source-file)) ""))
                           (cons "lineNumber" (or line-number 0)))))
    (when parent-id
      (setf data-alist (append data-alist (list (cons "parent" parent-id)))))
    (alist-to-hash-table
     (append
      (list (cons "group" "nodes")
            (cons "data" data-alist))
      (when position
        (list (cons "position" (list (cons "x" (gethash "x" position))
                                     (cons "y" (gethash "y" position))))))))))

(defun convert-edge-to-cytoscape (edge index)
  "Convert a Living Canvas edge to Cytoscape.js edge format."
  (let ((id (or (gethash "id" edge) (format nil "edge-~D" index)))
        (source (gethash "source" edge))
        (target (gethash "target" edge)))
    (alist-to-hash-table
     (list (cons "group" "edges")
           (cons "data" (list (cons "id" id)
                              (cons "source" source)
                              (cons "target" target)))))))

(defun convert-group-to-cytoscape (group)
  "Convert a Living Canvas group to Cytoscape.js parent node format."
  (let* ((id (gethash "id" group))
         (name (gethash "name" group))
         (type (gethash "type" group))
         (attributes (gethash "attributes" group))
         (filepath (when attributes (gethash "filepath" attributes))))
    (alist-to-hash-table
     (list (cons "group" "nodes")
           (cons "data" (list (cons "id" id)
                              (cons "name" name)
                              (cons "type" type)
                              (cons "filepath" (or filepath ""))))))))

(defun parse-living-canvas-json (json-string)
  "Parse Living Canvas JSON string into a hash table.
Returns NIL if parsing fails."
  (handler-case
      (yason:parse json-string)
    (error (c)
      (declare (ignore c))
      nil)))

(defun validate-living-canvas-json-p (parsed-json)
  "Validate parsed Living Canvas JSON structure.
Returns T if valid, NIL otherwise."
  (and (hash-table-p parsed-json)
       (gethash "version" parsed-json)
       (gethash "nodes" parsed-json)
       (gethash "edges" parsed-json)))

(defun convert-to-cytoscape (json-string)
  "Convert Living Canvas JSON to Cytoscape.js format.
Returns a JSON string ready for Cytoscape.js initialization."
  (let ((parsed (parse-living-canvas-json json-string)))
    (unless parsed
      (lem:editor-error "Failed to parse Living Canvas JSON"))
    (unless (validate-living-canvas-json-p parsed)
      (lem:editor-error "Invalid Living Canvas JSON structure"))
    (let ((elements '())
          (nodes (gethash "nodes" parsed))
          (edges (gethash "edges" parsed))
          (groups (gethash "groups" parsed)))
      ;; Add group nodes (file containers) first
      ;; Note: YASON returns lists, not vectors, so use :in instead of :across
      (when groups
        (loop :for group :in (coerce groups 'list)
              :do (push (convert-group-to-cytoscape group) elements)))
      ;; Add function nodes
      (loop :for node :in (coerce nodes 'list)
            :do (push (convert-node-to-cytoscape node) elements))
      ;; Add edges
      (loop :for edge :in (coerce edges 'list)
            :for i :from 0
            :do (push (convert-edge-to-cytoscape edge i) elements))
      ;; Generate JSON output
      (with-output-to-string (stream)
        (yason:encode
         (alexandria:plist-hash-table
          (list "elements" (coerce (nreverse elements) 'vector))
          :test 'equal)
         stream)))))
