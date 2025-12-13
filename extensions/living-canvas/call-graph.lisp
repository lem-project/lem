(in-package :lem-living-canvas/call-graph)

;;; Data Structures

(defstruct graph-node
  "Represents a function node in the call graph"
  (id "" :type string)
  (name "" :type string)
  (package "" :type string)
  (type :function :type keyword)
  (docstring nil :type (or null string))
  (source-location nil :type (or null cons))
  (position nil :type (or null cons)))

(defstruct graph-edge
  "Represents a call relationship between functions"
  (source "" :type string)
  (target "" :type string)
  (call-type :direct :type keyword))

(defstruct call-graph
  "Complete call graph for a package"
  (nodes (make-hash-table :test 'equal) :type hash-table)
  (edges nil :type list)
  (root-package nil))

;;; Source Location

(defun get-source-location (symbol)
  "Get the source file and line number for a symbol's function definition"
  (handler-case
      (let ((source (sb-introspect:find-definition-source
                     (fdefinition symbol))))
        (when source
          (let ((pathname (sb-introspect:definition-source-pathname source))
                (form-number (sb-introspect:definition-source-form-number source)))
            (when pathname
              (cons (namestring pathname)
                    (or form-number 1))))))
    (error () nil)))

;;; Function Analysis

(defun get-function-type (symbol)
  "Determine the type of a function symbol"
  (cond ((macro-function symbol) :macro)
        ((and (fboundp symbol)
              (typep (fdefinition symbol) 'generic-function))
         :generic-function)
        ((fboundp symbol) :function)
        (t nil)))

(defun extract-called-symbols (form package)
  "Extract all function symbols called within a form"
  (let ((calls '()))
    (labels ((walk (form)
               (cond
                 ;; Symbol reference
                 ((and (symbolp form)
                       (not (keywordp form))
                       (fboundp form))
                  (pushnew form calls))
                 ;; (function sym) or #'sym
                 ((and (consp form)
                       (eq (car form) 'function)
                       (symbolp (cadr form))
                       (fboundp (cadr form)))
                  (pushnew (cadr form) calls))
                 ;; Regular function call (sym ...)
                 ((consp form)
                  (let ((head (car form)))
                    ;; Check if head is a function call
                    (when (and (symbolp head)
                               (not (special-operator-p head))
                               (fboundp head))
                      (pushnew head calls))
                    ;; Walk arguments
                    (mapc #'walk (cdr form)))))))
      (handler-case
          (walk form)
        (error () nil)))
    calls))

(defun get-function-body (symbol)
  "Get the source form of a function if available"
  (handler-case
      (let ((source (sb-introspect:find-definition-source
                     (fdefinition symbol))))
        (when (and source
                   (sb-introspect:definition-source-pathname source))
          (let ((pathname (sb-introspect:definition-source-pathname source)))
            (when (probe-file pathname)
              (with-open-file (stream pathname)
                (let ((form-number (sb-introspect:definition-source-form-number source)))
                  ;; Read forms until we get to the right one
                  (loop :repeat (or form-number 0)
                        :do (read stream nil nil))
                  (read stream nil nil)))))))
    (error () nil)))

(defun get-callees (symbol package)
  "Get all functions called by a symbol within the given package"
  (let ((body (get-function-body symbol)))
    (when body
      (let ((all-calls (extract-called-symbols body package)))
        ;; Filter to only include symbols from the same package or exported
        (remove-if-not
         (lambda (sym)
           (eq (symbol-package sym) package))
         all-calls)))))

;;; Graph Construction

(defun make-node-id (symbol)
  "Create a unique ID for a node"
  (format nil "~A:~A"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defun analyze-package (package-designator)
  "Analyze a package and build its call graph"
  (let ((package (find-package package-designator)))
    (unless package
      (error "Package ~A not found" package-designator))
    (let ((nodes (make-hash-table :test 'equal))
          (edges '())
          (symbols '()))
      ;; First pass: collect all function symbols
      (do-symbols (sym package)
        (when (and (eq (symbol-package sym) package)
                   (fboundp sym)
                   (get-function-type sym))
          (push sym symbols)))
      ;; Second pass: create nodes
      (dolist (sym symbols)
        (let ((node-id (make-node-id sym)))
          (setf (gethash node-id nodes)
                (make-graph-node
                 :id node-id
                 :name (symbol-name sym)
                 :package (package-name package)
                 :type (get-function-type sym)
                 :docstring (documentation sym 'function)
                 :source-location (get-source-location sym)))))
      ;; Third pass: create edges
      (dolist (sym symbols)
        (let ((callees (get-callees sym package))
              (source-id (make-node-id sym)))
          (dolist (callee callees)
            (let ((target-id (make-node-id callee)))
              (when (gethash target-id nodes)
                (push (make-graph-edge
                       :source source-id
                       :target target-id
                       :call-type :direct)
                      edges))))))
      (make-call-graph
       :nodes nodes
       :edges (remove-duplicates edges
                                 :test (lambda (a b)
                                         (and (string= (graph-edge-source a)
                                                       (graph-edge-source b))
                                              (string= (graph-edge-target a)
                                                       (graph-edge-target b)))))
       :root-package package))))

(defun analyze-buffer (buffer)
  "Analyze a buffer and extract its call graph.
Currently defaults to CL-USER package."
  (declare (ignore buffer))
  (analyze-package :cl-user))

;;; JSON Serialization

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table for YASON encoding"
  (let ((ht (make-hash-table :test 'equal)))
    (loop :for (key . value) :in alist
          :do (setf (gethash key ht)
                    (if (and (consp value) (consp (car value)))
                        (alist-to-hash-table value)
                        value)))
    ht))

(defun graph-to-cytoscape-json (graph)
  "Convert a call-graph to Cytoscape.js compatible JSON string"
  (with-output-to-string (stream)
    (let ((elements '()))
      ;; Add nodes
      (maphash (lambda (id node)
                 (declare (ignore id))
                 (push (alist-to-hash-table
                        `(("group" . "nodes")
                          ("data" . (("id" . ,(graph-node-id node))
                                     ("name" . ,(graph-node-name node))
                                     ("type" . ,(string-downcase
                                                 (symbol-name (graph-node-type node))))
                                     ("package" . ,(graph-node-package node))
                                     ("docstring" . ,(or (graph-node-docstring node) ""))))))
                       elements))
               (call-graph-nodes graph))
      ;; Add edges
      (loop :for edge :in (call-graph-edges graph)
            :for i :from 0
            :do (push (alist-to-hash-table
                       `(("group" . "edges")
                         ("data" . (("id" . ,(format nil "edge-~D" i))
                                    ("source" . ,(graph-edge-source edge))
                                    ("target" . ,(graph-edge-target edge))))))
                      elements))
      ;; Write JSON
      (yason:encode
       (alexandria:plist-hash-table
        (list "elements" (coerce (nreverse elements) 'vector))
        :test 'equal)
       stream))))
