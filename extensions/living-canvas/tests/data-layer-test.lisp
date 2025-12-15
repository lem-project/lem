(defpackage :lem-living-canvas/tests/data-layer
  (:use :cl :rove)
  (:import-from :lem-living-canvas/call-graph
                #:make-graph-node
                #:make-graph-edge
                #:make-call-graph
                #:graph-node-id
                #:graph-node-name
                #:graph-node-package
                #:graph-node-type
                #:graph-node-docstring
                #:graph-node-arglist
                #:graph-node-source-location
                #:graph-node-source-file
                #:graph-node-position
                #:graph-edge-source
                #:graph-edge-target
                #:graph-edge-call-type
                #:call-graph-nodes
                #:call-graph-edges
                #:call-graph-root-package
                #:graph-to-json
                #:json-to-graph
                #:save-graph
                #:load-graph)
  (:import-from :lem-living-canvas/graph-format
                #:convert-to-cytoscape
                #:parse-living-canvas-json
                #:validate-living-canvas-json-p))
(in-package :lem-living-canvas/tests/data-layer)

;;; Test Fixtures

(defun make-test-graph ()
  "Create a simple test graph with 3 nodes and 2 edges."
  (let ((nodes (make-hash-table :test 'equal)))
    ;; Node 1: function
    (setf (gethash "TEST-PKG:FOO" nodes)
          (make-graph-node
           :id "TEST-PKG:FOO"
           :name "FOO"
           :package "TEST-PKG"
           :type :function
           :docstring "A test function"
           :arglist "(x y)"
           :source-location (cons "/test/file.lisp" 10)
           :source-file "/test/file.lisp"
           :position nil))
    ;; Node 2: macro
    (setf (gethash "TEST-PKG:BAR" nodes)
          (make-graph-node
           :id "TEST-PKG:BAR"
           :name "BAR"
           :package "TEST-PKG"
           :type :macro
           :docstring "A test macro"
           :arglist "(body)"
           :source-location (cons "/test/file.lisp" 20)
           :source-file "/test/file.lisp"
           :position (cons 100 200)))
    ;; Node 3: command (no docstring)
    (setf (gethash "TEST-PKG:BAZ" nodes)
          (make-graph-node
           :id "TEST-PKG:BAZ"
           :name "BAZ"
           :package "TEST-PKG"
           :type :command
           :docstring nil
           :arglist "()"
           :source-location (cons "/test/other.lisp" 5)
           :source-file "/test/other.lisp"
           :position nil))
    ;; Edges
    (let ((edges (list
                  (make-graph-edge
                   :source "TEST-PKG:FOO"
                   :target "TEST-PKG:BAR"
                   :call-type :direct)
                  (make-graph-edge
                   :source "TEST-PKG:BAR"
                   :target "TEST-PKG:BAZ"
                   :call-type :direct))))
      (make-call-graph
       :nodes nodes
       :edges edges
       :root-package nil))))

;;; graph-to-json Tests

(deftest test-graph-to-json-basic
  "Test basic graph-to-json conversion."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph)))
    (ok (stringp json) "Returns a string")
    (ok (> (length json) 0) "JSON is not empty")
    (ok (search "\"$schema\":\"living-canvas-graph-v1\"" json)
        "Contains schema identifier")
    (ok (search "\"version\":\"1.0\"" json)
        "Contains version")))

(deftest test-graph-to-json-nodes
  "Test that nodes are correctly serialized."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph)))
    (ok (search "\"TEST-PKG:FOO\"" json) "Contains FOO node ID")
    (ok (search "\"TEST-PKG:BAR\"" json) "Contains BAR node ID")
    (ok (search "\"TEST-PKG:BAZ\"" json) "Contains BAZ node ID")
    (ok (search "\"function\"" json) "Contains function type")
    (ok (search "\"macro\"" json) "Contains macro type")
    (ok (search "\"command\"" json) "Contains command type")))

(deftest test-graph-to-json-edges
  "Test that edges are correctly serialized."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph)))
    (ok (search "\"source\":\"TEST-PKG:FOO\"" json)
        "Contains edge source FOO")
    (ok (search "\"target\":\"TEST-PKG:BAR\"" json)
        "Contains edge target BAR")))

(deftest test-graph-to-json-groups
  "Test that file groups are correctly created."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph)))
    (ok (search "\"file.lisp\"" json) "Contains file.lisp group")
    (ok (search "\"other.lisp\"" json) "Contains other.lisp group")))

(deftest test-graph-to-json-metadata
  "Test that metadata is correctly generated."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph :scope-type "package" :scope-name "TEST-PKG")))
    (ok (search "\"generator\":\"lem-living-canvas\"" json)
        "Contains generator")
    (ok (search "\"type\":\"package\"" json)
        "Contains scope type")
    (ok (search "\"name\":\"TEST-PKG\"" json)
        "Contains scope name")))

(deftest test-graph-to-json-with-layout
  "Test that layout positions are included when requested."
  (let* ((graph (make-test-graph))
         (json-without (graph-to-json graph :include-layout nil))
         (json-with (graph-to-json graph :include-layout t)))
    ;; BAR has position (100 . 200)
    (ok (not (search "\"x\":100" json-without))
        "Without layout flag, no position data")
    (ok (search "\"x\":100" json-with)
        "With layout flag, position x is included")
    (ok (search "\"y\":200" json-with)
        "With layout flag, position y is included")))

;;; json-to-graph Tests

(deftest test-json-to-graph-roundtrip
  "Test that graph -> JSON -> graph preserves data."
  (let* ((original (make-test-graph))
         (json (graph-to-json original))
         (restored (json-to-graph json)))
    (ok (= (hash-table-count (call-graph-nodes original))
           (hash-table-count (call-graph-nodes restored)))
        "Same number of nodes")
    (ok (= (length (call-graph-edges original))
           (length (call-graph-edges restored)))
        "Same number of edges")))

(deftest test-json-to-graph-node-properties
  "Test that node properties are correctly restored."
  (let* ((original (make-test-graph))
         (json (graph-to-json original))
         (restored (json-to-graph json))
         (foo-node (gethash "TEST-PKG:FOO" (call-graph-nodes restored))))
    (ok foo-node "FOO node exists")
    (ok (equal (graph-node-name foo-node) "FOO") "Name preserved")
    (ok (equal (graph-node-package foo-node) "TEST-PKG") "Package preserved")
    (ok (eq (graph-node-type foo-node) :function) "Type preserved")
    (ok (equal (graph-node-docstring foo-node) "A test function")
        "Docstring preserved")
    (ok (equal (graph-node-arglist foo-node) "(x y)")
        "Arglist preserved")))

(deftest test-json-to-graph-edge-properties
  "Test that edge properties are correctly restored."
  (let* ((original (make-test-graph))
         (json (graph-to-json original))
         (restored (json-to-graph json))
         (edges (call-graph-edges restored)))
    (ok (= (length edges) 2) "Two edges restored")
    (let ((first-edge (find "TEST-PKG:FOO" edges
                            :key #'graph-edge-source
                            :test #'equal)))
      (ok first-edge "Edge from FOO found")
      (ok (equal (graph-edge-target first-edge) "TEST-PKG:BAR")
          "Edge target is BAR")
      (ok (eq (graph-edge-call-type first-edge) :direct)
          "Call type preserved"))))

(deftest test-json-to-graph-position-roundtrip
  "Test that positions are preserved through roundtrip."
  (let* ((original (make-test-graph))
         (json (graph-to-json original :include-layout t))
         (restored (json-to-graph json))
         (bar-node (gethash "TEST-PKG:BAR" (call-graph-nodes restored))))
    (ok bar-node "BAR node exists")
    (let ((pos (graph-node-position bar-node)))
      (ok pos "Position is present")
      (ok (= (car pos) 100) "X position preserved")
      (ok (= (cdr pos) 200) "Y position preserved"))))

(deftest test-json-to-graph-invalid-json
  "Test that invalid JSON raises an error."
  (ok (signals (json-to-graph "not valid json"))
      "Signals error for invalid JSON")
  (ok (signals (json-to-graph "{}"))
      "Signals error for empty object (missing nodes/edges)"))

;;; convert-to-cytoscape Tests

(deftest test-convert-to-cytoscape-basic
  "Test basic Cytoscape conversion."
  (let* ((graph (make-test-graph))
         (living-json (graph-to-json graph))
         (cytoscape-json (convert-to-cytoscape living-json)))
    (ok (stringp cytoscape-json) "Returns a string")
    (ok (search "\"elements\"" cytoscape-json)
        "Contains elements array")
    (ok (search "\"group\":\"nodes\"" cytoscape-json)
        "Contains node groups")
    (ok (search "\"group\":\"edges\"" cytoscape-json)
        "Contains edge groups")))

(deftest test-convert-to-cytoscape-node-data
  "Test that Cytoscape nodes have correct data format."
  (let* ((graph (make-test-graph))
         (living-json (graph-to-json graph))
         (cytoscape-json (convert-to-cytoscape living-json)))
    ;; Check node data fields
    (ok (search "\"id\":\"TEST-PKG:FOO\"" cytoscape-json)
        "Node ID in data")
    (ok (search "\"name\":\"FOO\"" cytoscape-json)
        "Node name in data")
    (ok (search "\"type\":\"function\"" cytoscape-json)
        "Node type in data")))

(deftest test-convert-to-cytoscape-parent-nodes
  "Test that file parent nodes are created."
  (let* ((graph (make-test-graph))
         (living-json (graph-to-json graph))
         (cytoscape-json (convert-to-cytoscape living-json)))
    (ok (search "\"type\":\"file\"" cytoscape-json)
        "File type nodes exist")
    (ok (search "\"parent\":" cytoscape-json)
        "Nodes have parent references")))

;;; parse-living-canvas-json Tests

(deftest test-parse-living-canvas-json-valid
  "Test parsing valid JSON."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph))
         (parsed (parse-living-canvas-json json)))
    (ok (hash-table-p parsed) "Returns a hash table")
    (ok (gethash "version" parsed) "Has version field")
    (ok (gethash "nodes" parsed) "Has nodes field")
    (ok (gethash "edges" parsed) "Has edges field")))

(deftest test-parse-living-canvas-json-invalid
  "Test parsing invalid JSON returns NIL."
  (ok (null (parse-living-canvas-json "not json"))
      "Returns NIL for invalid JSON")
  (ok (null (parse-living-canvas-json "{broken"))
      "Returns NIL for broken JSON"))

;;; validate-living-canvas-json-p Tests

(deftest test-validate-living-canvas-json-p-valid
  "Test validation of valid JSON."
  (let* ((graph (make-test-graph))
         (json (graph-to-json graph))
         (parsed (parse-living-canvas-json json)))
    (ok (validate-living-canvas-json-p parsed)
        "Valid JSON passes validation")))

(deftest test-validate-living-canvas-json-p-invalid
  "Test validation of invalid structures."
  (ok (not (validate-living-canvas-json-p nil))
      "NIL fails validation")
  (ok (not (validate-living-canvas-json-p (make-hash-table)))
      "Empty hash table fails validation")
  (let ((incomplete (make-hash-table :test 'equal)))
    (setf (gethash "version" incomplete) "1.0")
    (ok (not (validate-living-canvas-json-p incomplete))
        "Missing nodes/edges fails validation")))

;;; File I/O Tests

(deftest test-save-and-load-graph
  "Test saving and loading graph to/from file."
  (let* ((original (make-test-graph))
         (temp-file (format nil "/tmp/test-graph-~A.json" (get-universal-time))))
    (unwind-protect
        (progn
          ;; Save
          (save-graph original temp-file)
          (ok (probe-file temp-file) "File was created")
          ;; Load
          (let ((loaded (load-graph temp-file)))
            (ok loaded "Graph was loaded")
            (ok (= (hash-table-count (call-graph-nodes original))
                   (hash-table-count (call-graph-nodes loaded)))
                "Loaded graph has same node count")))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(deftest test-load-graph-nonexistent-file
  "Test that loading nonexistent file raises error."
  (ok (signals (load-graph "/nonexistent/path/file.json"))
      "Signals error for nonexistent file"))
