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
  (source-file nil :type (or null string))
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

(defun line-defines-symbol-p (line symbol-name)
  "Check if a line defines the given symbol"
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (dolist (prefix '("(defun " "(defmacro " "(defgeneric " "(defmethod "))
      (let ((prefix-len (length prefix)))
        (when (and (>= (length trimmed) prefix-len)
                   (string= prefix (subseq trimmed 0 prefix-len)))
          (let* ((rest (subseq trimmed prefix-len))
                 (space-pos (position #\Space rest))
                 (paren-pos (position #\( rest))
                 (end-pos (or space-pos paren-pos (length rest)))
                 (name (subseq rest 0 end-pos)))
            (when (string-equal name symbol-name)
              (return-from line-defines-symbol-p t))))))
    nil))

(defun find-definition-line (pathname symbol-name)
  "Find the line number where a symbol is defined"
  (when (and pathname (probe-file pathname))
    (with-open-file (stream pathname)
      (loop :for line-number :from 1
            :for line = (read-line stream nil nil)
            :while line
            :when (line-defines-symbol-p line symbol-name)
              :return line-number))))

(defun get-source-location (symbol)
  "Get the source file and line number for a symbol's function definition"
  (handler-case
      (let ((source (sb-introspect:find-definition-source
                     (fdefinition symbol))))
        (when source
          (let ((pathname (sb-introspect:definition-source-pathname source)))
            (when pathname
              (let ((line (find-definition-line pathname (symbol-name symbol))))
                (cons (namestring pathname)
                      (or line 1)))))))
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

(defun read-all-forms-from-file (pathname)
  "Read all forms from a file, handling in-package correctly"
  (handler-case
      (with-open-file (stream pathname)
        (let ((*package* (find-package :cl-user))
              (*read-eval* nil)
              (forms '()))
          (loop :for form = (read stream nil :eof)
                :until (eq form :eof)
                :do (when (and (consp form)
                               (eq (car form) 'in-package))
                      (let ((pkg (find-package (cadr form))))
                        (when pkg (setf *package* pkg))))
                    (push form forms))
          (nreverse forms)))
    (error () nil)))

(defun find-definition-in-forms (symbol forms)
  "Find the defun/defmacro form that defines symbol"
  (loop :for form :in forms
        :when (and (consp form)
                   (member (car form) '(defun defmacro defgeneric defmethod))
                   (eq (cadr form) symbol))
          :return form))

(defun get-function-body (symbol)
  "Get the source form of a function if available"
  (handler-case
      ;; First try function-lambda-expression for interpreted functions
      (let ((lambda-expr (function-lambda-expression (fdefinition symbol))))
        (if lambda-expr
            lambda-expr
            ;; Fall back to reading source file
            (let ((source (sb-introspect:find-definition-source
                           (fdefinition symbol))))
              (when (and source
                         (sb-introspect:definition-source-pathname source))
                (let ((pathname (sb-introspect:definition-source-pathname source)))
                  (when (probe-file pathname)
                    (let* ((forms (read-all-forms-from-file pathname))
                           (defn (find-definition-in-forms symbol forms)))
                      defn)))))))
    (error () nil)))

(defun get-callees (symbol package)
  "Get all functions called by a symbol within the given package"
  (let ((body (get-function-body symbol)))
    (when body
      (let ((all-calls (extract-called-symbols body package)))
        ;; Filter to only include symbols from the same package
        ;; and exclude self-references
        (remove-if
         (lambda (sym)
           (or (eq sym symbol)
               (not (eq (symbol-package sym) package))))
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
        (let* ((node-id (make-node-id sym))
               (source-loc (get-source-location sym))
               (source-file (when source-loc (car source-loc))))
          (setf (gethash node-id nodes)
                (make-graph-node
                 :id node-id
                 :name (symbol-name sym)
                 :package (package-name package)
                 :type (get-function-type sym)
                 :docstring (documentation sym 'function)
                 :source-location source-loc
                 :source-file source-file))))
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

(defun extract-definitions-from-file (pathname)
  "Extract all function/macro definitions from a file"
  (let ((forms (read-all-forms-from-file pathname))
        (definitions '()))
    (dolist (form forms)
      (when (and (consp form)
                 (member (car form) '(defun defmacro defgeneric)))
        (let ((name (cadr form)))
          (when (and (symbolp name) (fboundp name))
            (push name definitions)))))
    (nreverse definitions)))

(defun analyze-file (pathname)
  "Analyze a single file and build its call graph"
  (let ((definitions (extract-definitions-from-file pathname))
        (nodes (make-hash-table :test 'equal))
        (edges '())
        (file-path (namestring pathname)))
    (when definitions
      (let ((package (symbol-package (first definitions))))
        ;; Create nodes for all definitions in the file
        (dolist (sym definitions)
          (let* ((node-id (make-node-id sym))
                 (source-loc (get-source-location sym)))
            (setf (gethash node-id nodes)
                  (make-graph-node
                   :id node-id
                   :name (symbol-name sym)
                   :package (package-name (symbol-package sym))
                   :type (get-function-type sym)
                   :docstring (documentation sym 'function)
                   :source-location source-loc
                   :source-file (or (car source-loc) file-path)))))
        ;; Create edges (only between functions in this file)
        (dolist (sym definitions)
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
         :root-package package)))))

(defun analyze-buffer (buffer)
  "Analyze a buffer and extract its call graph"
  (let ((pathname (lem:buffer-filename buffer)))
    (if (and pathname (probe-file pathname))
        (or (analyze-file pathname)
            ;; Fallback if no definitions found
            (make-call-graph :nodes (make-hash-table :test 'equal)
                            :edges nil
                            :root-package nil))
        (make-call-graph :nodes (make-hash-table :test 'equal)
                        :edges nil
                        :root-package nil))))

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

(defun make-file-node-id (filepath)
  "Create a unique ID for a file node"
  (format nil "file:~A" (or filepath "unknown")))

(defun short-filename (filepath)
  "Extract just the filename from a path"
  (if filepath
      (file-namestring filepath)
      "unknown"))

(defun graph-to-cytoscape-json (graph)
  "Convert a call-graph to Cytoscape.js compatible JSON string"
  (with-output-to-string (stream)
    (let ((elements '())
          (files (make-hash-table :test 'equal)))
      ;; First pass: collect unique source files
      (maphash (lambda (id node)
                 (declare (ignore id))
                 (let ((source-file (graph-node-source-file node)))
                   (when source-file
                     (setf (gethash source-file files) t))))
               (call-graph-nodes graph))
      ;; Add file parent nodes
      (maphash (lambda (filepath _)
                 (declare (ignore _))
                 (push (alist-to-hash-table
                        `(("group" . "nodes")
                          ("data" . (("id" . ,(make-file-node-id filepath))
                                     ("name" . ,(short-filename filepath))
                                     ("type" . "file")
                                     ("filepath" . ,filepath)))))
                       elements))
               files)
      ;; Add function nodes with parent reference
      (maphash (lambda (id node)
                 (declare (ignore id))
                 (let ((source-file (graph-node-source-file node)))
                   (push (alist-to-hash-table
                          `(("group" . "nodes")
                            ("data" . (("id" . ,(graph-node-id node))
                                       ("name" . ,(graph-node-name node))
                                       ("type" . ,(string-downcase
                                                   (symbol-name (graph-node-type node))))
                                       ("package" . ,(graph-node-package node))
                                       ("docstring" . ,(or (graph-node-docstring node) ""))
                                       ,@(when source-file
                                           `(("parent" . ,(make-file-node-id source-file))))))))
                         elements)))
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
