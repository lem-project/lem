(defpackage :lem-living-canvas/cl-provider
  (:use :cl :call-graph)
  (:export #:cl-call-graph-provider
           #:analyze-package
           #:analyze-file
           #:analyze-buffer
           #:analyze-system
           #:get-source-location))
(in-package :lem-living-canvas/cl-provider)

;;; Common Lisp Call Graph Provider
;;;
;;; This provider uses SBCL's sb-introspect for analyzing Common Lisp code.
;;; It extracts function definitions and call relationships from loaded code.

(defclass cl-call-graph-provider (call-graph-provider)
  ()
  (:documentation "Call graph provider for Common Lisp using sb-introspect."))

(defmethod provider-name ((provider cl-call-graph-provider))
  :common-lisp)

(defmethod provider-priority ((provider cl-call-graph-provider))
  0)

(defmethod provider-supports-p ((provider cl-call-graph-provider) source)
  "Support Lem buffers in lisp-mode and Lisp source files."
  (or (and (typep source 'lem:buffer)
           (eq (lem:buffer-major-mode source) 'lem-lisp-mode:lisp-mode))
      (and (or (stringp source) (pathnamep source))
           (string-equal "lisp" (pathname-type (pathname source))))))

(defmethod provider-analyze ((provider cl-call-graph-provider) source &key type)
  "Analyze SOURCE and return a call-graph.
TYPE can be :buffer, :file, :package, or :system."
  (cond
    ((eq type :package)
     (analyze-package source))
    ((eq type :system)
     (analyze-system source))
    ((typep source 'lem:buffer)
     (analyze-buffer source))
    ((or (stringp source) (pathnamep source))
     (analyze-file source))
    (t
     (make-call-graph))))

;;; Configuration for Definition Detection

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
  "Definition forms to exclude from call graph analysis.")

;;; Source Location Detection

(defun line-defines-symbol-p (line symbol-name)
  "Check if a line defines the given symbol."
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
    ;; Lem-specific definition forms
    (dolist (prefix '("(define-command " "(lem:define-command "
                      "(define-major-mode " "(lem:define-major-mode "
                      "(define-minor-mode " "(lem:define-minor-mode "))
      (let ((prefix-len (length prefix)))
        (when (and (>= (length trimmed) prefix-len)
                   (string-equal prefix (subseq trimmed 0 prefix-len)))
          (let* ((rest (subseq trimmed prefix-len))
                 (rest-trimmed (string-left-trim '(#\Space #\Tab) rest)))
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
  "Find the line number where a symbol is defined."
  (when (and pathname (probe-file pathname))
    (with-open-file (stream pathname)
      (loop :for line-number :from 1
            :for line = (read-line stream nil nil)
            :while line
            :when (line-defines-symbol-p line symbol-name)
              :return line-number))))

(defun get-source-location (symbol)
  "Get the source file and line number for a symbol's function definition."
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
  "Check if FORM-HEAD is a definition macro."
  (and (symbolp form-head)
       (let ((name (symbol-name form-head)))
         (or (and (>= (length name) 3)
                  (string-equal "DEF" name :end2 3))
             (and (>= (length name) 7)
                  (string-equal "DEFINE-" name :end2 7))))))

(defun extract-definition-name (form)
  "Extract the defined name from a definition form."
  (when (and (consp form) (consp (cdr form)))
    (let ((form-head (car form))
          (second-element (cadr form)))
      (cond
        ((member form-head '(define-command define-major-mode define-minor-mode
                             define-global-mode) :test #'string-equal)
         (if (consp second-element)
             (car second-element)
             second-element))
        ((symbolp second-element)
         second-element)
        (t nil)))))

(defun excluded-definition-form-p (form-head)
  "Check if FORM-HEAD should be excluded from call graph analysis."
  (member (symbol-name form-head)
          (mapcar #'symbol-name *excluded-definition-forms*)
          :test #'string-equal))

;;; Function Analysis

(defun get-function-type (symbol)
  "Determine the type of a function symbol."
  (cond ((macro-function symbol) :macro)
        ((and (fboundp symbol)
              (typep (fdefinition symbol) 'generic-function))
         :generic-function)
        ((fboundp symbol) :function)
        (t nil)))

(defun get-definition-type (symbol &optional form-head)
  "Determine the type of a definition, including Lem-specific types."
  (cond
    ((and form-head (member (symbol-name form-head) '("DEFINE-MAJOR-MODE") :test #'string-equal))
     :major-mode)
    ((and form-head (member (symbol-name form-head) '("DEFINE-MINOR-MODE") :test #'string-equal))
     :minor-mode)
    ((and form-head (member (symbol-name form-head) '("DEFINE-COMMAND") :test #'string-equal))
     :command)
    ((macro-function symbol) :macro)
    ((and (fboundp symbol)
          (typep (fdefinition symbol) 'generic-function))
     :generic-function)
    ((fboundp symbol) :function)
    (t nil)))

(defun format-arglist-for-display (symbol)
  "Format the argument list for display."
  (handler-case
      (let ((arglist (sb-introspect:function-lambda-list
                      (if (macro-function symbol)
                          (macro-function symbol)
                          (fdefinition symbol)))))
        (when arglist
          (format nil "(~{~(~A~)~^ ~})" arglist)))
    (error () nil)))

;;; Source Code Reading

(defun read-all-forms-from-file (pathname)
  "Read all forms from a file, handling in-package correctly."
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

;;; Source-based Call Detection

(defun extract-called-symbols-from-form (form target-symbols)
  "Extract symbols from FORM that are in TARGET-SYMBOLS set."
  (let ((calls '()))
    (labels ((walk (f)
               (cond
                 ((and (symbolp f)
                       (gethash f target-symbols))
                  (pushnew f calls))
                 ((and (consp f)
                       (eq (car f) 'function)
                       (symbolp (cadr f))
                       (gethash (cadr f) target-symbols))
                  (pushnew (cadr f) calls))
                 ((consp f)
                  (walk (car f))
                  (walk (cdr f))))))
      (handler-case
          (walk form)
        (error () nil)))
    calls))

(defun get-definition-body (form)
  "Extract the body from a definition form."
  (when (and (consp form) (consp (cdr form)))
    (let ((head (car form)))
      (cond
        ((member head '(defun defmacro) :test #'string-equal)
         (cdddr form))
        ((member head '(define-command lem:define-command) :test #'string-equal)
         (cdddr form))
        (t (cdddr form))))))

(defun extract-source-calls (pathname target-symbols)
  "Extract call relationships from source file PATHNAME."
  (let ((forms (read-all-forms-from-file pathname))
        (calls '()))
    (dolist (form forms)
      (when (and (consp form)
                 (def-form-p (car form))
                 (not (excluded-definition-form-p (car form))))
        (let ((name (extract-definition-name form))
              (body (get-definition-body form)))
          (when (and name (symbolp name) (gethash name target-symbols))
            (let ((callees (extract-called-symbols-from-form body target-symbols)))
              (dolist (callee callees)
                (unless (eq callee name)
                  (push (cons name callee) calls))))))))
    calls))

;;; Introspection Helpers

(defun function-to-symbol (fn)
  "Try to get the symbol name for a function object."
  (handler-case
      (let ((name (sb-kernel:%fun-name fn)))
        (cond
          ((symbolp name) name)
          ((and (consp name) (eq (car name) 'sb-pcl::fast-method))
           (cadr name))
          ((and (consp name) (member (car name) '(setf sb-pcl::slot-accessor)))
           nil)
          (t nil)))
    (error () nil)))

(defun get-callees (symbol package)
  "Get all functions called by SYMBOL within the given package."
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
  "Get all functions that call SYMBOL within the given package."
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

(defun symbol-to-node-id (symbol)
  "Create a unique ID for a symbol node."
  (make-node-id (package-name (symbol-package symbol))
                (symbol-name symbol)))

(defun extract-definitions-from-file (pathname)
  "Extract all function/macro definitions from a file."
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

;;; Analysis Functions

(defun analyze-package (package-designator)
  "Analyze a package and build its call graph."
  (let ((package (find-package package-designator)))
    (unless package
      (lem:editor-error "Package ~A not found" package-designator))
    (let ((graph (make-call-graph :root-package package))
          (symbols '()))
      ;; Collect all function symbols
      (do-symbols (sym package)
        (when (and (eq (symbol-package sym) package)
                   (fboundp sym)
                   (get-function-type sym))
          (push sym symbols)))
      ;; Create nodes
      (dolist (sym symbols)
        (let* ((node-id (symbol-to-node-id sym))
               (source-loc (get-source-location sym))
               (source-file (when source-loc (car source-loc))))
          (add-node graph
                    (make-graph-node
                     :id node-id
                     :name (symbol-name sym)
                     :package (package-name package)
                     :type (get-function-type sym)
                     :docstring (documentation sym 'function)
                     :arglist (format-arglist-for-display sym)
                     :source-location source-loc
                     :source-file source-file))))
      ;; Create edges using who-calls
      (dolist (sym symbols)
        (let ((callers (get-callers sym package))
              (target-id (symbol-to-node-id sym)))
          (dolist (caller callers)
            (let ((source-id (symbol-to-node-id caller)))
              (when (find-node graph source-id)
                (add-edge graph
                          (make-graph-edge
                           :source source-id
                           :target target-id
                           :call-type :direct)))))))
      ;; Also try find-function-callees
      (dolist (sym symbols)
        (let ((callees (get-callees sym package))
              (source-id (symbol-to-node-id sym)))
          (dolist (callee callees)
            (let ((target-id (symbol-to-node-id callee)))
              (when (find-node graph target-id)
                (add-edge graph
                          (make-graph-edge
                           :source source-id
                           :target target-id
                           :call-type :direct)))))))
      ;; Remove duplicate edges
      (setf (call-graph-edges graph)
            (remove-duplicate-edges (call-graph-edges graph)))
      graph)))

(defun analyze-file (pathname)
  "Analyze a single file and build its call graph."
  (let ((definitions (extract-definitions-from-file pathname))
        (graph (make-call-graph))
        (file-path (namestring pathname))
        (symbols '())
        (symbol-set (make-hash-table :test 'eq)))
    (when definitions
      (let ((package (symbol-package (car (first definitions)))))
        (setf (call-graph-root-package graph) package)
        ;; Create nodes
        (dolist (def definitions)
          (let* ((sym (car def))
                 (form-head (cdr def))
                 (node-id (symbol-to-node-id sym))
                 (source-loc (get-source-location sym)))
            (push sym symbols)
            (setf (gethash sym symbol-set) t)
            (add-node graph
                      (make-graph-node
                       :id node-id
                       :name (symbol-name sym)
                       :package (package-name (symbol-package sym))
                       :type (get-definition-type sym form-head)
                       :docstring (documentation sym 'function)
                       :arglist (format-arglist-for-display sym)
                       :source-location source-loc
                       :source-file (or (car source-loc) file-path)))))
        ;; Create edges using sb-introspect
        (dolist (sym symbols)
          (let ((callers (get-callers sym package))
                (target-id (symbol-to-node-id sym)))
            (dolist (caller callers)
              (let ((source-id (symbol-to-node-id caller)))
                (when (find-node graph source-id)
                  (add-edge graph
                            (make-graph-edge
                             :source source-id
                             :target target-id
                             :call-type :direct)))))))
        (dolist (sym symbols)
          (let ((callees (get-callees sym package))
                (source-id (symbol-to-node-id sym)))
            (dolist (callee callees)
              (let ((target-id (symbol-to-node-id callee)))
                (when (find-node graph target-id)
                  (add-edge graph
                            (make-graph-edge
                             :source source-id
                             :target target-id
                             :call-type :direct)))))))
        ;; Source-based call detection
        (let ((source-calls (extract-source-calls pathname symbol-set)))
          (dolist (call source-calls)
            (let ((source-id (symbol-to-node-id (car call)))
                  (target-id (symbol-to-node-id (cdr call))))
              (when (and (find-node graph source-id)
                         (find-node graph target-id))
                (add-edge graph
                          (make-graph-edge
                           :source source-id
                           :target target-id
                           :call-type :direct))))))
        ;; Remove duplicate edges
        (setf (call-graph-edges graph)
              (remove-duplicate-edges (call-graph-edges graph)))))
    graph))

(defun analyze-buffer (buffer)
  "Analyze a buffer and extract its call graph."
  (let ((pathname (lem:buffer-filename buffer)))
    (if (and pathname (probe-file pathname))
        (analyze-file pathname)
        (make-call-graph))))

;;; ASDF System Analysis

(defun get-system-source-files (system-designator)
  "Get all Lisp source files from an ASDF system."
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
                      (collect-files child)))
                   (otherwise nil))))
        (collect-files system))
      (nreverse files))))

(defun analyze-system (system-designator)
  "Analyze an ASDF system and build its call graph."
  (let ((files (get-system-source-files system-designator))
        (graph (make-call-graph))
        (all-symbols '())
        (symbol-set (make-hash-table :test 'eq)))
    (unless files
      (error "No source files found in system ~A" system-designator))
    ;; Collect all definitions
    (dolist (file files)
      (let ((definitions (extract-definitions-from-file file)))
        (dolist (def definitions)
          (let* ((sym (car def))
                 (form-head (cdr def))
                 (node-id (symbol-to-node-id sym))
                 (source-loc (get-source-location sym)))
            (unless (find-node graph node-id)
              (push sym all-symbols)
              (setf (gethash sym symbol-set) t)
              (add-node graph
                        (make-graph-node
                         :id node-id
                         :name (symbol-name sym)
                         :package (package-name (symbol-package sym))
                         :type (get-definition-type sym form-head)
                         :docstring (documentation sym 'function)
                         :arglist (format-arglist-for-display sym)
                         :source-location source-loc
                         :source-file (or (car source-loc) file))))))))
    ;; Create edges using sb-introspect
    (dolist (sym all-symbols)
      (let ((source-id (symbol-to-node-id sym)))
        (handler-case
            (let ((fn (fdefinition sym)))
              (when fn
                (let ((callees (sb-introspect:find-function-callees fn)))
                  (dolist (callee callees)
                    (let ((callee-sym (function-to-symbol callee)))
                      (when callee-sym
                        (let ((target-id (symbol-to-node-id callee-sym)))
                          (when (and (not (equal source-id target-id))
                                     (find-node graph target-id))
                            (add-edge graph
                                      (make-graph-edge
                                       :source source-id
                                       :target target-id
                                       :call-type :direct))))))))))
          (error () nil))))
    ;; Source-based call detection
    (dolist (file files)
      (let ((source-calls (extract-source-calls file symbol-set)))
        (dolist (call source-calls)
          (let ((source-id (symbol-to-node-id (car call)))
                (target-id (symbol-to-node-id (cdr call))))
            (when (and (find-node graph source-id)
                       (find-node graph target-id))
              (add-edge graph
                        (make-graph-edge
                         :source source-id
                         :target target-id
                         :call-type :direct)))))))
    ;; Remove duplicate edges
    (setf (call-graph-edges graph)
          (remove-duplicate-edges (call-graph-edges graph)))
    graph))
