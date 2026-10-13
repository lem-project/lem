(defpackage #:lem-living-canvas/micros-cl-provider
  (:use #:cl #:call-graph)
  (:import-from #:lem-lisp-mode/internal
                #:connected-p
                #:check-connection
                #:lisp-eval)
  (:export #:micros-cl-call-graph-provider
           #:analyze-package
           #:analyze-file
           #:analyze-buffer
           #:analyze-system))
(in-package #:lem-living-canvas/micros-cl-provider)

;;; Micros-based Common Lisp Call Graph Provider
;;;
;;; This provider uses micros RPC to analyze Common Lisp code on the
;;; connected runtime (either self-connection or remote).

(defclass micros-cl-call-graph-provider (call-graph-provider)
  ()
  (:documentation "Call graph provider for Common Lisp using micros RPC.
Works with both self-connection and remote connections."))

(defmethod provider-name ((provider micros-cl-call-graph-provider))
  :micros-common-lisp)

(defmethod provider-priority ((provider micros-cl-call-graph-provider))
  10)  ; Higher priority than local provider

(defmethod provider-supports-p ((provider micros-cl-call-graph-provider) source)
  "Support Lem buffers in lisp-mode and Lisp source files when connected."
  (and (connected-p)
       (or (and (typep source 'lem:buffer)
                (eq (lem:buffer-major-mode source) 'lem-lisp-mode:lisp-mode))
           (and (or (stringp source) (pathnamep source))
                (string-equal "lisp" (pathname-type (pathname source)))))))

(defmethod provider-analyze ((provider micros-cl-call-graph-provider) source &key type)
  "Analyze SOURCE and return a call-graph.
TYPE can be :buffer, :file, :package, or :system."
  (check-connection)
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

;;; Plist to Call-Graph Conversion

(defun plist-to-graph-node (plist)
  "Convert a plist to a graph-node structure."
  (make-graph-node
   :id (getf plist :id)
   :name (getf plist :name)
   :package (getf plist :package)
   :type (getf plist :type)
   :docstring (getf plist :docstring)
   :arglist (getf plist :arglist)
   :source-location (when (getf plist :source-file)
                      (cons (getf plist :source-file)
                            (getf plist :source-line)))
   :source-file (getf plist :source-file)))

(defun plist-to-graph-edge (plist)
  "Convert a plist to a graph-edge structure."
  (make-graph-edge
   :source (getf plist :source)
   :target (getf plist :target)
   :call-type (or (getf plist :call-type) :direct)))

(defun plist-to-call-graph (plist)
  "Convert a plist representation to a call-graph structure."
  (let ((graph (make-call-graph)))
    (dolist (node-plist (getf plist :nodes))
      (add-node graph (plist-to-graph-node node-plist)))
    (dolist (edge-plist (getf plist :edges))
      (add-edge graph (plist-to-graph-edge edge-plist)))
    graph))

;;; Analysis Functions

(defun analyze-package (package-designator)
  "Analyze a package on the connected runtime and build its call graph."
  (check-connection)
  (let ((result (lisp-eval `(micros:call-graph-analyze-package ,package-designator))))
    (if result
        (plist-to-call-graph result)
        (make-call-graph))))

(defun analyze-file (pathname)
  "Analyze a file on the connected runtime and build its call graph."
  (check-connection)
  (let ((path (namestring pathname)))
    (let ((result (lisp-eval `(micros:call-graph-analyze-file ,path))))
      (if result
          (plist-to-call-graph result)
          (make-call-graph)))))

(defun analyze-buffer (buffer)
  "Analyze a buffer and extract its call graph."
  (let ((pathname (lem:buffer-filename buffer)))
    (if (and pathname (probe-file pathname))
        (analyze-file pathname)
        (make-call-graph))))

(defun analyze-system (system-designator)
  "Analyze an ASDF system on the connected runtime and build its call graph."
  (check-connection)
  (let ((result (lisp-eval `(micros:call-graph-analyze-system ,system-designator))))
    (if result
        (plist-to-call-graph result)
        (make-call-graph))))
