(defpackage :lem-living-canvas/call-graph
  (:use :cl)
  (:export #:graph-node
           #:make-graph-node
           #:graph-node-id
           #:graph-node-name
           #:graph-node-package
           #:graph-node-type
           #:graph-node-docstring
           #:graph-node-arglist
           #:graph-node-source-location
           #:graph-node-source-file
           #:graph-node-position
           #:graph-edge
           #:make-graph-edge
           #:graph-edge-source
           #:graph-edge-target
           #:graph-edge-call-type
           #:call-graph
           #:make-call-graph
           #:call-graph-nodes
           #:call-graph-edges
           #:call-graph-root-package
           #:analyze-package
           #:analyze-file
           #:analyze-buffer
           #:analyze-system
           #:graph-to-cytoscape-json
           #:get-source-location))
(in-package :lem-living-canvas/call-graph)

;;; Configuration for Definition Detection
;;;
;;; Living Canvas analyzes source files to extract function definitions.
;;; Not all `def*` forms represent callable functions - variables, types,
;;; and configuration forms should be excluded from the call graph.

(defparameter *excluded-definition-forms*
  '(;; Package/module structure
    defpackage in-package defsystem
    ;; Variable definitions (fboundp = false)
    defvar defparameter defconstant
    ;; Type definitions
    defclass defstruct deftype define-condition
    ;; Testing
    deftest
    ;; Lem-specific non-function definitions
    define-editor-variable define-attribute
    ;; Key bindings (not functions)
    define-keys define-key define-named-key)
  "Definition forms to exclude from call graph analysis.
These forms either don't create callable functions (variables, types)
or are not relevant to understanding code flow (package declarations, tests).")

(defparameter *unknown-source-id* "file:(unknown source)"
  "Sentinel ID for grouping functions whose source file cannot be determined.
Used as a parent node in the Cytoscape graph visualization.")

;;; Data Structures

(defstruct graph-node
  "Represents a function node in the call graph"
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
  "Check if a line defines the given symbol.
Supports standard CL forms (defun, defmacro, etc.) and Lem-specific forms
(define-command, define-major-mode, define-minor-mode)."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    ;; Standard CL definition forms
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
    ;; Lem-specific definition forms (may have name or (name options...))
    ;; Support both unqualified and package-qualified forms (lem:define-command, etc.)
    (dolist (prefix '("(define-command " "(lem:define-command "
                      "(define-major-mode " "(lem:define-major-mode "
                      "(define-minor-mode " "(lem:define-minor-mode "))
      (let ((prefix-len (length prefix)))
        (when (and (>= (length trimmed) prefix-len)
                   (string-equal prefix (subseq trimmed 0 prefix-len)))
          (let* ((rest (subseq trimmed prefix-len))
                 (rest-trimmed (string-left-trim '(#\Space #\Tab) rest)))
            ;; Handle (name options...) case
            (when (and (> (length rest-trimmed) 0)
                       (char= (char rest-trimmed 0) #\())
              (setf rest-trimmed (subseq rest-trimmed 1)))
            (let* ((space-pos (position #\Space rest-trimmed))
                   (paren-pos (position #\( rest-trimmed))
                   (close-paren-pos (position #\) rest-trimmed))
                   (end-pos (or space-pos paren-pos close-paren-pos (length rest-trimmed)))
                   (name (subseq rest-trimmed 0 end-pos)))
              (when (string-equal name symbol-name)
                (return-from line-defines-symbol-p t)))))))
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

;;; Definition Form Detection

(defun def-form-p (form-head)
  "Check if FORM-HEAD is a definition macro (starts with DEF or DEFINE-).
This enables Living Canvas to recognize Lem-specific macros like define-command
alongside standard Common Lisp definition forms like defun."
  (and (symbolp form-head)
       (let ((name (symbol-name form-head)))
         (or (and (>= (length name) 3)
                  (string-equal "DEF" name :end2 3))
             (and (>= (length name) 7)
                  (string-equal "DEFINE-" name :end2 7))))))

(defun extract-definition-name (form)
  "Extract the defined name from a definition form.
Different definition macros have different syntax for specifying the name:
- Standard CL: (defun NAME ...) - name is second element
- Lem commands: (define-command NAME ...) or (define-command (NAME options) ...)
Returns a symbol or NIL if the form structure is unrecognized."
  (when (and (consp form) (consp (cdr form)))
    (let ((form-head (car form))
          (second-element (cadr form)))
      (cond
        ;; define-command: (define-command (name options...) ...) or (define-command name ...)
        ((member form-head '(define-command define-major-mode define-minor-mode
                             define-global-mode) :test #'string-equal)
         (if (consp second-element)
             (car second-element)
             second-element))
        ;; Standard forms: (defun name ...), etc.
        ((symbolp second-element)
         second-element)
        (t nil)))))

;;; Function Analysis

(defun get-function-type (symbol)
  "Determine the type of a function symbol"
  (cond ((macro-function symbol) :macro)
        ((and (fboundp symbol)
              (typep (fdefinition symbol) 'generic-function))
         :generic-function)
        ((fboundp symbol) :function)
        (t nil)))

(defun get-definition-type (symbol &optional form-head)
  "Determine the type of a definition, including Lem-specific types.
SYMBOL is the defined symbol.
FORM-HEAD is the definition macro used (e.g., DEFUN, DEFINE-COMMAND).
Returns :command, :major-mode, :minor-mode, :macro, :generic-function, :function, or NIL.

Lem-specific types are identified by the definition macro name (form-head)
rather than runtime properties, allowing analysis of unloaded code."
  (cond
    ;; Lem-specific types (identified by definition macro name)
    ;; Check modes first because define-major-mode internally creates a command too
    ((and form-head (member (symbol-name form-head) '("DEFINE-MAJOR-MODE") :test #'string-equal))
     :major-mode)
    ((and form-head (member (symbol-name form-head) '("DEFINE-MINOR-MODE") :test #'string-equal))
     :minor-mode)
    ((and form-head (member (symbol-name form-head) '("DEFINE-COMMAND") :test #'string-equal))
     :command)
    ;; Standard CL types (runtime introspection)
    ((macro-function symbol) :macro)
    ((and (fboundp symbol)
          (typep (fdefinition symbol) 'generic-function))
     :generic-function)
    ((fboundp symbol) :function)
    (t nil)))

(defun format-arglist-for-display (symbol)
  "Format the argument list for display in the UI.
Returns a formatted string like '(x y &optional z)' or nil."
  (handler-case
      (let ((arglist (sb-introspect:function-lambda-list
                      (if (macro-function symbol)
                          (macro-function symbol)
                          (fdefinition symbol)))))
        (when arglist
          (format nil "(~{~(~A~)~^ ~})" arglist)))
    (error () nil)))

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

(defun function-to-symbol (fn)
  "Try to get the symbol name for a function object"
  (handler-case
      (let ((name (sb-kernel:%fun-name fn)))
        (cond
          ;; Regular function
          ((symbolp name) name)
          ;; Method - extract generic function name
          ((and (consp name) (eq (car name) 'sb-pcl::fast-method))
           (cadr name))
          ((and (consp name) (member (car name) '(setf sb-pcl::slot-accessor)))
           nil) ; Skip setf functions and slot accessors
          (t nil)))
    (error () nil)))

(defun get-callees (symbol package)
  "Get all functions called by SYMBOL within the given package.
Uses sb-introspect:find-function-callees for compiled code analysis."
  (handler-case
      (let ((fn (fdefinition symbol)))
        (when fn
          (let ((callees (sb-introspect:find-function-callees fn))
                (result '()))
            (dolist (callee callees)
              (let ((callee-sym (function-to-symbol callee)))
                (when (and callee-sym
                           (not (eq callee-sym symbol))
                           (eq (symbol-package callee-sym) package)
                           (fboundp callee-sym))
                  (pushnew callee-sym result))))
            result)))
    (error () nil)))

(defun get-callers (symbol package)
  "Get all functions that call SYMBOL within the given package.
Uses sb-introspect:who-calls which relies on xref data from compilation."
  (handler-case
      (let ((xrefs (sb-introspect:who-calls symbol))
            (result '()))
        (dolist (xref xrefs)
          (let ((caller-name (car xref)))
            (when (and (symbolp caller-name)
                       (not (eq caller-name symbol))
                       (eq (symbol-package caller-name) package)
                       (fboundp caller-name))
              (pushnew caller-name result))))
        result)
    (error () nil)))

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
                 :arglist (format-arglist-for-display sym)
                 :source-location source-loc
                 :source-file source-file))))
      ;; Third pass: create edges using who-calls (xref data)
      ;; For each symbol, find what calls it and create edges
      (dolist (sym symbols)
        (let ((callers (get-callers sym package))
              (target-id (make-node-id sym)))
          (dolist (caller callers)
            (let ((source-id (make-node-id caller)))
              (when (gethash source-id nodes)
                (push (make-graph-edge
                       :source source-id
                       :target target-id
                       :call-type :direct)
                      edges))))))
      ;; Also try find-function-callees as backup
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

(defun excluded-definition-form-p (form-head)
  "Check if FORM-HEAD should be excluded from call graph analysis.
Returns T for definition forms that don't create callable functions
(e.g., defvar, defclass) or aren't relevant to code flow analysis."
  (member (symbol-name form-head)
          (mapcar #'symbol-name *excluded-definition-forms*)
          :test #'string-equal))

(defun extract-definitions-from-file (pathname)
  "Extract all function/macro definitions from a file.
Returns a list of (symbol . form-head) pairs for each definition found."
  (let ((forms (read-all-forms-from-file pathname))
        (definitions '()))
    (dolist (form forms)
      (when (and (consp form)
                 (def-form-p (car form))
                 (not (excluded-definition-form-p (car form))))
        (let ((name (extract-definition-name form)))
          (when (and name (symbolp name) (fboundp name))
            (push (cons name (car form)) definitions)))))
    (nreverse definitions)))

(defun analyze-file (pathname)
  "Analyze a single file and build its call graph"
  (let ((definitions (extract-definitions-from-file pathname))
        (nodes (make-hash-table :test 'equal))
        (edges '())
        (file-path (namestring pathname))
        (symbols '()))  ; Store just symbols for edge analysis
    (when definitions
      (let ((package (symbol-package (car (first definitions)))))
        ;; Create nodes for all definitions in the file
        ;; definitions is a list of (symbol . form-head) pairs
        (dolist (def definitions)
          (let* ((sym (car def))
                 (form-head (cdr def))
                 (node-id (make-node-id sym))
                 (source-loc (get-source-location sym)))
            (push sym symbols)
            (setf (gethash node-id nodes)
                  (make-graph-node
                   :id node-id
                   :name (symbol-name sym)
                   :package (package-name (symbol-package sym))
                   :type (get-definition-type sym form-head)
                   :docstring (documentation sym 'function)
                   :arglist (format-arglist-for-display sym)
                   :source-location source-loc
                   :source-file (or (car source-loc) file-path)))))
        ;; Create edges (only between functions in this file)
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

;;; ASDF System Analysis

(defun get-system-source-files (system-designator)
  "Get all Lisp source files from an ASDF system"
  (let ((system (asdf:find-system system-designator nil)))
    (unless system
      (error "System ~A not found" system-designator))
    (let ((files '()))
      (labels ((collect-files (component)
                 (typecase component
                   (asdf:cl-source-file
                    (let ((path (asdf:component-pathname component)))
                      (when (and path (probe-file path))
                        (push (namestring path) files))))
                   (asdf:module
                    (dolist (child (asdf:component-children component))
                      (collect-files child)))
                   (asdf:system
                    (dolist (child (asdf:component-children component))
                      (collect-files child))))))
        (collect-files system))
      (nreverse files))))

(defun analyze-system (system-designator)
  "Analyze an ASDF system and build its call graph.
Shows all functions defined in the system and their call relationships."
  (let ((files (get-system-source-files system-designator))
        (nodes (make-hash-table :test 'equal))
        (edges '())
        (all-symbols '()))
    (unless files
      (error "No source files found in system ~A" system-designator))
    ;; First pass: collect all function definitions from all files
    ;; definitions is a list of (symbol . form-head) pairs
    (dolist (file files)
      (let ((definitions (extract-definitions-from-file file)))
        (dolist (def definitions)
          (let* ((sym (car def))
                 (form-head (cdr def))
                 (node-id (make-node-id sym))
                 (source-loc (get-source-location sym)))
            (unless (gethash node-id nodes)
              (push sym all-symbols)
              (setf (gethash node-id nodes)
                    (make-graph-node
                     :id node-id
                     :name (symbol-name sym)
                     :package (package-name (symbol-package sym))
                     :type (get-definition-type sym form-head)
                     :docstring (documentation sym 'function)
                     :arglist (format-arglist-for-display sym)
                     :source-location source-loc
                     :source-file (or (car source-loc) file))))))))
    ;; Second pass: create edges (cross-package calls allowed)
    (dolist (sym all-symbols)
      (let ((source-id (make-node-id sym)))
        ;; Use find-function-callees for call relationships
        (handler-case
            (let ((fn (fdefinition sym)))
              (when fn
                (let ((callees (sb-introspect:find-function-callees fn)))
                  (dolist (callee callees)
                    (let ((callee-sym (function-to-symbol callee)))
                      (when callee-sym
                        (let ((target-id (make-node-id callee-sym)))
                          (when (and (not (equal source-id target-id))
                                     (gethash target-id nodes))
                            (push (make-graph-edge
                                   :source source-id
                                   :target target-id
                                   :call-type :direct)
                                  edges)))))))))
          (error () nil))))
    (make-call-graph
     :nodes nodes
     :edges (remove-duplicates edges
                               :test (lambda (a b)
                                       (and (string= (graph-edge-source a)
                                                     (graph-edge-source b))
                                            (string= (graph-edge-target a)
                                                     (graph-edge-target b)))))
     :root-package nil)))

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
                        `(("group" . "nodes")
                          ("data" . (("id" . ,(make-file-node-id filepath))
                                     ("name" . ,(short-filename filepath))
                                     ("type" . "file")
                                     ("filepath" . ,filepath)))))
                       elements))
               files)
      ;; Add unknown source container if there are orphan nodes
      (when has-orphans
        (push (alist-to-hash-table
               `(("group" . "nodes")
                 ("data" . (("id" . ,*unknown-source-id*)
                            ("name" . "(unknown source)")
                            ("type" . "file")
                            ("filepath" . "")))))
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
                          `(("group" . "nodes")
                            ("data" . (("id" . ,(graph-node-id node))
                                       ("name" . ,(graph-node-name node))
                                       ("type" . ,(string-downcase
                                                   (symbol-name (graph-node-type node))))
                                       ("package" . ,(graph-node-package node))
                                       ("docstring" . ,(or (graph-node-docstring node) ""))
                                       ("arglist" . ,(or (graph-node-arglist node) ""))
                                       ("sourceFile" . ,(or (short-filename source-file) ""))
                                       ("lineNumber" . ,(or line-number 0))
                                       ("parent" . ,parent-id)))))
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
