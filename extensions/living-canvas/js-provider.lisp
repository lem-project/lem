(defpackage #:lem-living-canvas/javascript
  (:use #:cl)
  (:local-nicknames (:ts :tree-sitter))
  (:import-from #:call-graph
                #:call-graph-provider
                #:provider-name
                #:provider-priority
                #:provider-languages
                #:provider-supports-p
                #:provider-analyze
                #:make-call-graph
                #:make-graph-node
                #:make-graph-edge
                #:add-node
                #:add-edge
                #:make-node-id
                #:*provider-registry*
                #:register-provider
                #:find-provider)
  (:export #:tree-sitter-js-provider
           #:make-js-provider))
(in-package #:lem-living-canvas/javascript)

;;; Tree-sitter JavaScript/TypeScript Provider
;;;
;;; This provider uses tree-sitter-cl to parse JavaScript and TypeScript source code
;;; and extract function definitions and call relationships.

;;; Tree-sitter Query Definitions
;;;
;;; These queries extract function definitions and calls from JS/TS AST.

(defparameter *js-definitions-query*
  "(function_declaration
     name: (identifier) @function.name) @function

   (lexical_declaration
     (variable_declarator
       name: (identifier) @arrow.name
       value: (arrow_function))) @arrow

   (variable_declaration
     (variable_declarator
       name: (identifier) @arrow.name
       value: (arrow_function))) @arrow

   (method_definition
     name: (property_identifier) @method.name) @method

   (class_declaration
     name: (identifier) @class.name) @class"
  "Tree-sitter query for JavaScript function, arrow function, and method definitions.")

(defparameter *js-calls-query*
  "(call_expression
     function: (identifier) @call.function)

   (call_expression
     function: (member_expression
       property: (property_identifier) @call.method))"
  "Tree-sitter query for JavaScript function and method calls.")

;;; Provider Class

(defclass tree-sitter-js-provider (call-graph-provider)
  ((language :initform nil
             :accessor provider-ts-language
             :documentation "Cached tree-sitter JavaScript language object")
   (parser :initform nil
           :accessor provider-ts-parser
           :documentation "Cached tree-sitter parser instance")
   (definitions-query :initform nil
                      :accessor provider-definitions-query
                      :documentation "Compiled definitions query")
   (calls-query :initform nil
                :accessor provider-calls-query
                :documentation "Compiled calls query"))
  (:documentation "Call graph provider for JavaScript/TypeScript using tree-sitter parsing.

Uses tree-sitter-cl bindings to parse JavaScript source code and extract
function declarations, arrow functions, methods, and call relationships.
Also works with TypeScript since TS is a superset of JS."))

(defmethod initialize-instance :after ((provider tree-sitter-js-provider) &key)
  "Initialize tree-sitter language and compile queries."
  (when (ts:tree-sitter-available-p)
    (handler-case
        (progn
          ;; Load JavaScript grammar
          (unless (ts:get-language "javascript")
            (ts:load-language-from-system "javascript"))
          (let ((lang (ts:get-language "javascript")))
            (setf (provider-ts-language provider) lang)
            (setf (provider-ts-parser provider) (ts:make-parser lang))
            ;; Compile queries
            (setf (provider-definitions-query provider)
                  (ts:query-compile lang *js-definitions-query*))
            (setf (provider-calls-query provider)
                  (ts:query-compile lang *js-calls-query*))))
      (error (e)
        (warn "Failed to initialize JavaScript tree-sitter provider: ~A" e)))))

;;; Provider Protocol Implementation

(defmethod provider-name ((provider tree-sitter-js-provider))
  :tree-sitter-javascript)

(defmethod provider-priority ((provider tree-sitter-js-provider))
  5)  ; Lower than LSP providers (10), higher than default (0)

(defmethod provider-languages ((provider tree-sitter-js-provider))
  '(:javascript :typescript))

(defmethod provider-supports-p ((provider tree-sitter-js-provider) source)
  "Return T if SOURCE is a JavaScript or TypeScript file or buffer."
  (and (provider-ts-language provider)  ; Check provider is initialized
       (typecase source
         (pathname
          (member (pathname-type source)
                  '("js" "mjs" "cjs" "jsx" "ts" "mts" "cts" "tsx")
                  :test #'string-equal))
         (string t)  ; Accept raw source code strings
         (t
          ;; For Lem buffers, check major mode or filename
          (when (and (find-package :lem)
                     (typep source (find-symbol "BUFFER" :lem)))
            (let ((mode-name (string-downcase
                              (symbol-name
                               (funcall (find-symbol "BUFFER-MAJOR-MODE" :lem) source)))))
              (or (string= mode-name "js-mode")
                  (string= mode-name "typescript-mode")
                  (let ((filename (funcall (find-symbol "BUFFER-FILENAME" :lem) source)))
                    (and filename
                         (member (pathname-type (pathname filename))
                                 '("js" "mjs" "cjs" "jsx" "ts" "mts" "cts" "tsx")
                                 :test #'string-equal))))))))))

(defmethod provider-analyze ((provider tree-sitter-js-provider) source &key type)
  "Analyze SOURCE and return a call-graph structure.

Arguments:
  SOURCE - JavaScript/TypeScript source code (string, pathname, or buffer)
  TYPE   - Optional type hint (:string, :file, :buffer)

Returns:
  A call-graph structure with nodes for functions/methods and edges for calls"
  (let ((text (extract-source-text source type))
        (source-file (extract-source-file source type)))
    (if text
        (parse-and-extract-graph provider text source-file)
        (make-call-graph))))

;;; Source Extraction

(defun extract-source-text (source type)
  "Extract source code text from SOURCE.

Arguments:
  SOURCE - String, pathname, or Lem buffer
  TYPE   - Type hint (:string, :file, :buffer) or nil for auto-detect

Returns:
  Source code as a string, or NIL if extraction fails"
  (cond
    ((eq type :string) source)
    ((stringp source)
     (if (probe-file source)
         (uiop:read-file-string source)
         source))
    ((pathnamep source)
     (when (probe-file source)
       (uiop:read-file-string source)))
    ((and (find-package :lem)
          (typep source (find-symbol "BUFFER" :lem)))
     (funcall (find-symbol "BUFFER-TEXT" :lem) source))
    (t nil)))

(defun extract-source-file (source type)
  "Extract source file path from SOURCE.

Returns:
  File path as string, or NIL if not applicable"
  (cond
    ((eq type :string) nil)
    ((pathnamep source) (namestring source))
    ((stringp source)
     (when (probe-file source) source))
    ((and (find-package :lem)
          (typep source (find-symbol "BUFFER" :lem)))
     (funcall (find-symbol "BUFFER-FILENAME" :lem) source))
    (t nil)))

;;; Parsing and Graph Construction

(defun parse-and-extract-graph (provider text source-file)
  "Parse TEXT with tree-sitter and extract call graph.

Arguments:
  PROVIDER    - The JavaScript provider instance
  TEXT        - Source code string
  SOURCE-FILE - Optional source file path for location info

Returns:
  A call-graph structure"
  (let ((graph (make-call-graph))
        (parser (provider-ts-parser provider))
        (def-query (provider-definitions-query provider))
        (call-query (provider-calls-query provider)))
    (when (and parser def-query call-query)
      (let* ((tree (ts:parser-parse-string parser text))
             (root (ts:tree-root-node tree)))
        ;; Pass 1: Extract function definitions
        (let ((definitions (extract-definitions root def-query source-file text)))
          (dolist (def definitions)
            (add-node graph def))
          ;; Pass 2: Extract call relationships
          (let ((edges (extract-calls root call-query definitions text)))
            (dolist (edge edges)
              (add-edge graph edge))))))
    graph))

(defun extract-definitions (root query source-file source-text)
  "Extract function and method definitions from AST.

Arguments:
  ROOT        - Tree-sitter root node
  QUERY       - Compiled definitions query
  SOURCE-FILE - Source file path for location info
  SOURCE-TEXT - Original source code text for extracting node text

Returns:
  List of graph-node structures"
  (let ((nodes nil)
        (captures (ts:query-captures query root)))
    (dolist (capture captures)
      (let ((node (ts:capture-node capture))
            (name (ts:capture-name capture)))
        (cond
          ((string= name "function.name")
           (push (make-function-node node source-file :function source-text) nodes))
          ((string= name "arrow.name")
           (push (make-function-node node source-file :function source-text) nodes))
          ((string= name "method.name")
           (push (make-function-node node source-file :method source-text) nodes))
          ((string= name "class.name")
           (push (make-class-node node source-file source-text) nodes)))))
    (remove-duplicates nodes :key #'call-graph:graph-node-id :test #'string=)))

(defun extract-js-parameters (node source-text)
  "Extract parameters from a JavaScript function node.

Arguments:
  NODE        - Tree-sitter node (parent of the name identifier)
  SOURCE-TEXT - Original source code text

Returns:
  Parameter string like \"(x, y)\" or \"()\" if no parameters"
  (let* ((node-type (ts:node-type node))
         (params-node
           (cond
             ;; function_declaration, method_definition -> formal_parameters
             ((or (string= node-type "function_declaration")
                  (string= node-type "method_definition"))
              (find-child-by-type node "formal_parameters"))
             ;; variable_declarator with arrow_function
             ((string= node-type "variable_declarator")
              (let ((arrow (find-child-by-type node "arrow_function")))
                (when arrow
                  (find-child-by-type arrow "formal_parameters"))))
             (t nil))))
    (if params-node
        (node-text params-node source-text)
        "()")))

(defun make-function-node (name-node source-file type source-text)
  "Create a graph-node for a function or method definition.

Arguments:
  NAME-NODE   - Tree-sitter node for the function name
  SOURCE-FILE - Source file path
  TYPE        - :function or :method
  SOURCE-TEXT - Original source code text

Returns:
  A graph-node structure"
  (let* ((name (node-text name-node source-text))
         (start-point (ts:node-start-point name-node))
         (line (1+ (ts:ts-point-row start-point)))
         ;; Get parent node to extract parameters
         (parent (ts:node-parent name-node))
         (arglist (when parent (extract-js-parameters parent source-text))))
    (make-graph-node
     :id (make-node-id "module" name)
     :name name
     :package "module"
     :type type
     :arglist (or arglist "()")
     :source-file source-file
     :source-location (when source-file (cons source-file line)))))

(defun make-class-node (name-node source-file source-text)
  "Create a graph-node for a class definition.

Arguments:
  NAME-NODE   - Tree-sitter node for the class name
  SOURCE-FILE - Source file path
  SOURCE-TEXT - Original source code text

Returns:
  A graph-node structure"
  (let* ((name (node-text name-node source-text))
         (start-point (ts:node-start-point name-node))
         (line (1+ (ts:ts-point-row start-point))))
    (make-graph-node
     :id (make-node-id "module" name)
     :name name
     :package "module"
     :type :class
     :source-file source-file
     :source-location (when source-file (cons source-file line)))))

(defun extract-calls (root query definitions text)
  "Extract function call relationships from AST.

Arguments:
  ROOT        - Tree-sitter root node
  QUERY       - Compiled calls query
  DEFINITIONS - List of defined function nodes
  TEXT        - Source text for context lookup

Returns:
  List of graph-edge structures"
  (let ((edges nil)
        (def-names (mapcar #'call-graph:graph-node-name definitions))
        (captures (ts:query-captures query root)))
    (dolist (capture captures)
      (let ((node (ts:capture-node capture))
            (name (ts:capture-name capture)))
        (cond
          ((string= name "call.function")
           (let* ((called-name (node-text node text))
                  (caller (find-enclosing-function node definitions text)))
             (when (and caller (member called-name def-names :test #'string=))
               (push (make-graph-edge
                      :source (call-graph:graph-node-id caller)
                      :target (make-node-id "module" called-name))
                     edges))))
          ((string= name "call.method")
           ;; Method calls - track if calling a known method
           (let* ((called-name (node-text node text))
                  (caller (find-enclosing-function node definitions text)))
             (when (and caller (member called-name def-names :test #'string=))
               (push (make-graph-edge
                      :source (call-graph:graph-node-id caller)
                      :target (make-node-id "module" called-name))
                     edges)))))))
    (remove-duplicates edges
                       :test (lambda (a b)
                               (and (string= (call-graph:graph-edge-source a)
                                             (call-graph:graph-edge-source b))
                                    (string= (call-graph:graph-edge-target a)
                                             (call-graph:graph-edge-target b)))))))

(defun find-enclosing-function (node definitions source-text)
  "Find the function definition that encloses NODE.

Arguments:
  NODE        - Tree-sitter node to find enclosing function for
  DEFINITIONS - List of known function nodes
  SOURCE-TEXT - Original source code text

Returns:
  The enclosing graph-node, or NIL if not in a function"
  (let ((parent (ts:node-parent node)))
    (loop :while (and parent (not (ts:node-null-p parent)))
          :do (let ((parent-type (ts:node-type parent)))
                (when (or (string= parent-type "function_declaration")
                          (string= parent-type "arrow_function")
                          (string= parent-type "method_definition"))
                  ;; Find the name
                  (let ((name-node (or (find-child-by-type parent "identifier")
                                       (find-child-by-type parent "property_identifier"))))
                    (when name-node
                      (let* ((name (node-text name-node source-text))
                             (def (find name definitions
                                        :key #'call-graph:graph-node-name
                                        :test #'string=)))
                        (when def
                          (return-from find-enclosing-function def))))))
                ;; For arrow functions in variable declarations, check the parent
                (when (string= parent-type "variable_declarator")
                  (let ((name-node (find-child-by-type parent "identifier")))
                    (when name-node
                      (let* ((name (node-text name-node source-text))
                             (def (find name definitions
                                        :key #'call-graph:graph-node-name
                                        :test #'string=)))
                        (when def
                          (return-from find-enclosing-function def)))))))
              (setf parent (ts:node-parent parent)))
    nil))

(defun find-child-by-type (node type-name)
  "Find first child of NODE with given TYPE-NAME.

Arguments:
  NODE      - Tree-sitter node
  TYPE-NAME - Node type name to find

Returns:
  Child node or NIL if not found"
  (let ((count (ts:node-child-count node)))
    (loop :for i :below count
          :for child := (ts:node-child node i)
          :when (string= (ts:node-type child) type-name)
            :return child)))

(defun node-text (node source-text)
  "Get the source text for a tree-sitter node.

Arguments:
  NODE        - Tree-sitter node
  SOURCE-TEXT - Original source code string

Returns:
  Text string of the node"
  (let ((start-byte (ts:node-start-byte node))
        (end-byte (ts:node-end-byte node)))
    (subseq source-text start-byte end-byte)))

;;; Provider Registration
;;;
;;; Automatically register the JavaScript provider when this module is loaded

(defun make-js-provider ()
  "Create and return a new JavaScript tree-sitter provider instance."
  (make-instance 'tree-sitter-js-provider))

(defun register-js-provider ()
  "Register the JavaScript provider with the global registry.

Called both at load time and via after-init-hook to handle cases
where tree-sitter grammars become available after initial load."
  (when (ts:tree-sitter-available-p)
    (handler-case
        (let ((provider (make-js-provider)))
          (when (provider-ts-language provider)
            ;; Only register if not already registered
            (unless (find-provider *provider-registry* :javascript)
              (register-provider *provider-registry* provider '(:javascript :typescript)))))
      (error (e)
        (warn "Failed to register JavaScript provider: ~A" e)))))

;; Try to register on load (may fail if grammars not yet available)
(register-js-provider)

;; Also register after Lem init (grammars should be available by then)
(lem:add-hook lem:*after-init-hook* 'register-js-provider)
