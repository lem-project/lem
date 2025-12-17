(defpackage #:call-graph/tests/types
  (:use #:cl #:rove #:call-graph))
(in-package #:call-graph/tests/types)

;;; graph-node tests

(deftest make-graph-node-default
  (testing "creates node with default values"
    (let ((node (make-graph-node)))
      (ok (string= "" (graph-node-id node)))
      (ok (string= "" (graph-node-name node)))
      (ok (string= "" (graph-node-package node)))
      (ok (eq :function (graph-node-type node)))
      (ok (null (graph-node-docstring node)))
      (ok (null (graph-node-arglist node)))
      (ok (null (graph-node-source-location node)))
      (ok (null (graph-node-source-file node)))
      (ok (null (graph-node-position node))))))

(deftest make-graph-node-with-values
  (testing "creates node with specified values"
    (let ((node (make-graph-node
                 :id "TEST-PKG:FOO"
                 :name "FOO"
                 :package "TEST-PKG"
                 :type :macro
                 :docstring "A test macro"
                 :arglist "(x y)"
                 :source-location '("/path/to/file.lisp" . 42)
                 :source-file "/path/to/file.lisp")))
      (ok (string= "TEST-PKG:FOO" (graph-node-id node)))
      (ok (string= "FOO" (graph-node-name node)))
      (ok (string= "TEST-PKG" (graph-node-package node)))
      (ok (eq :macro (graph-node-type node)))
      (ok (string= "A test macro" (graph-node-docstring node)))
      (ok (string= "(x y)" (graph-node-arglist node)))
      (ok (equal '("/path/to/file.lisp" . 42) (graph-node-source-location node)))
      (ok (string= "/path/to/file.lisp" (graph-node-source-file node))))))

;;; graph-edge tests

(deftest make-graph-edge-default
  (testing "creates edge with default values"
    (let ((edge (make-graph-edge)))
      (ok (string= "" (graph-edge-source edge)))
      (ok (string= "" (graph-edge-target edge)))
      (ok (eq :direct (graph-edge-call-type edge))))))

(deftest make-graph-edge-with-values
  (testing "creates edge with specified values"
    (let ((edge (make-graph-edge
                 :source "PKG:CALLER"
                 :target "PKG:CALLEE"
                 :call-type :indirect)))
      (ok (string= "PKG:CALLER" (graph-edge-source edge)))
      (ok (string= "PKG:CALLEE" (graph-edge-target edge)))
      (ok (eq :indirect (graph-edge-call-type edge))))))

;;; call-graph tests

(deftest make-call-graph-default
  (testing "creates graph with default values"
    (let ((graph (make-call-graph)))
      (ok (hash-table-p (call-graph-nodes graph)))
      (ok (zerop (hash-table-count (call-graph-nodes graph))))
      (ok (null (call-graph-edges graph)))
      (ok (null (call-graph-root-package graph))))))

;;; make-node-id tests

(deftest make-node-id-creates-qualified-name
  (testing "creates PACKAGE:SYMBOL format ID"
    (ok (string= "MY-PKG:MY-FUNC" (make-node-id "MY-PKG" "MY-FUNC")))
    (ok (string= "CL-USER:TEST" (make-node-id "CL-USER" "TEST")))))

;;; add-node tests

(deftest add-node-adds-to-graph
  (testing "adds node to graph's nodes hash table"
    (let ((graph (make-call-graph))
          (node (make-graph-node :id "PKG:FOO" :name "FOO")))
      (add-node graph node)
      (ok (= 1 (hash-table-count (call-graph-nodes graph))))
      (ok (eq node (gethash "PKG:FOO" (call-graph-nodes graph)))))))

(deftest add-node-returns-node
  (testing "returns the added node"
    (let ((graph (make-call-graph))
          (node (make-graph-node :id "PKG:BAR")))
      (ok (eq node (add-node graph node))))))

(deftest add-node-overwrites-existing
  (testing "overwrites node with same ID"
    (let ((graph (make-call-graph))
          (node1 (make-graph-node :id "PKG:FOO" :name "FOO"))
          (node2 (make-graph-node :id "PKG:FOO" :name "FOO-UPDATED")))
      (add-node graph node1)
      (add-node graph node2)
      (ok (= 1 (hash-table-count (call-graph-nodes graph))))
      (ok (string= "FOO-UPDATED" (graph-node-name (gethash "PKG:FOO" (call-graph-nodes graph))))))))

;;; add-edge tests

(deftest add-edge-adds-to-graph
  (testing "adds edge to graph's edges list"
    (let ((graph (make-call-graph))
          (edge (make-graph-edge :source "PKG:A" :target "PKG:B")))
      (add-edge graph edge)
      (ok (= 1 (length (call-graph-edges graph))))
      (ok (eq edge (first (call-graph-edges graph)))))))

(deftest add-edge-returns-edge
  (testing "returns the added edge"
    (let ((graph (make-call-graph))
          (edge (make-graph-edge :source "PKG:X" :target "PKG:Y")))
      (ok (eq edge (add-edge graph edge))))))

(deftest add-edge-accumulates
  (testing "multiple edges accumulate in list"
    (let ((graph (make-call-graph)))
      (add-edge graph (make-graph-edge :source "A" :target "B"))
      (add-edge graph (make-graph-edge :source "B" :target "C"))
      (add-edge graph (make-graph-edge :source "A" :target "C"))
      (ok (= 3 (length (call-graph-edges graph)))))))

;;; find-node tests

(deftest find-node-returns-node
  (testing "finds node by ID"
    (let ((graph (make-call-graph))
          (node (make-graph-node :id "PKG:TARGET" :name "TARGET")))
      (add-node graph node)
      (ok (eq node (find-node graph "PKG:TARGET"))))))

(deftest find-node-returns-nil-for-missing
  (testing "returns nil for non-existent ID"
    (let ((graph (make-call-graph)))
      (ok (null (find-node graph "PKG:NONEXISTENT"))))))

;;; remove-duplicate-edges tests

(deftest remove-duplicate-edges-removes-duplicates
  (testing "removes edges with same source and target"
    (let ((edges (list (make-graph-edge :source "A" :target "B")
                       (make-graph-edge :source "A" :target "B")
                       (make-graph-edge :source "B" :target "C"))))
      (let ((result (remove-duplicate-edges edges)))
        (ok (= 2 (length result)))))))

(deftest remove-duplicate-edges-preserves-unique
  (testing "preserves all unique edges"
    (let ((edges (list (make-graph-edge :source "A" :target "B")
                       (make-graph-edge :source "B" :target "A")
                       (make-graph-edge :source "A" :target "C"))))
      (let ((result (remove-duplicate-edges edges)))
        (ok (= 3 (length result)))))))

(deftest remove-duplicate-edges-handles-empty
  (testing "handles empty list"
    (ok (null (remove-duplicate-edges '())))))

;;; graph-to-cytoscape-json tests

(deftest graph-to-cytoscape-json-empty-graph
  (testing "handles empty graph"
    (let* ((graph (make-call-graph))
           (result (graph-to-cytoscape-json graph)))
      (ok (hash-table-p result))
      (ok (gethash "elements" result))
      (ok (zerop (length (gethash "elements" result)))))))

(deftest graph-to-cytoscape-json-with-nodes
  (testing "includes nodes in output"
    (let ((graph (make-call-graph)))
      (add-node graph (make-graph-node
                       :id "PKG:FOO"
                       :name "FOO"
                       :package "PKG"
                       :type :function
                       :source-file "/path/test.lisp"))
      (let* ((result (graph-to-cytoscape-json graph))
             (elements (gethash "elements" result)))
        ;; Should have file node + function node
        (ok (>= (length elements) 2))))))

(deftest graph-to-cytoscape-json-with-edges
  (testing "includes edges in output"
    (let ((graph (make-call-graph)))
      (add-node graph (make-graph-node
                       :id "PKG:A"
                       :name "A"
                       :package "PKG"
                       :source-file "/test.lisp"))
      (add-node graph (make-graph-node
                       :id "PKG:B"
                       :name "B"
                       :package "PKG"
                       :source-file "/test.lisp"))
      (add-edge graph (make-graph-edge :source "PKG:A" :target "PKG:B"))
      (let* ((result (graph-to-cytoscape-json graph))
             (elements (gethash "elements" result))
             (edge-elements (remove-if-not
                             (lambda (e)
                               (string= "edges" (gethash "group" e)))
                             (coerce elements 'list))))
        (ok (= 1 (length edge-elements)))))))

(deftest graph-to-cytoscape-json-with-encoder
  (testing "uses provided encoder function"
    (let ((graph (make-call-graph))
          (encoder-called nil))
      (add-node graph (make-graph-node :id "PKG:X" :name "X" :package "PKG"))
      (graph-to-cytoscape-json
       graph
       :encoder (lambda (data)
                  (setf encoder-called t)
                  data))
      (ok encoder-called))))

;;; Integration test

(deftest integration-build-graph
  (testing "can build complete graph with nodes and edges"
    (let ((graph (make-call-graph)))
      ;; Add nodes
      (add-node graph (make-graph-node
                       :id "MY-PKG:MAIN"
                       :name "MAIN"
                       :package "MY-PKG"
                       :type :function
                       :docstring "Entry point"
                       :arglist "()"
                       :source-file "/src/main.lisp"
                       :source-location '("/src/main.lisp" . 10)))
      (add-node graph (make-graph-node
                       :id "MY-PKG:HELPER"
                       :name "HELPER"
                       :package "MY-PKG"
                       :type :function
                       :arglist "(x)"
                       :source-file "/src/main.lisp"
                       :source-location '("/src/main.lisp" . 20)))
      (add-node graph (make-graph-node
                       :id "MY-PKG:WITH-LOGGING"
                       :name "WITH-LOGGING"
                       :package "MY-PKG"
                       :type :macro
                       :arglist "(&body body)"
                       :source-file "/src/macros.lisp"
                       :source-location '("/src/macros.lisp" . 5)))
      ;; Add edges
      (add-edge graph (make-graph-edge :source "MY-PKG:MAIN" :target "MY-PKG:HELPER"))
      (add-edge graph (make-graph-edge :source "MY-PKG:MAIN" :target "MY-PKG:WITH-LOGGING"))
      ;; Verify structure
      (ok (= 3 (hash-table-count (call-graph-nodes graph))))
      (ok (= 2 (length (call-graph-edges graph))))
      ;; Verify node lookups
      (ok (find-node graph "MY-PKG:MAIN"))
      (ok (find-node graph "MY-PKG:HELPER"))
      (ok (find-node graph "MY-PKG:WITH-LOGGING"))
      (ok (null (find-node graph "MY-PKG:NONEXISTENT")))
      ;; Verify JSON conversion works
      (let ((json (graph-to-cytoscape-json graph)))
        (ok (hash-table-p json))
        (ok (gethash "elements" json))))))
