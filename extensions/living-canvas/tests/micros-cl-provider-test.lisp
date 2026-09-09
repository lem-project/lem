(defpackage #:lem-living-canvas/tests/micros-cl-provider
  (:use #:cl #:rove #:call-graph)
  (:import-from #:lem-living-canvas/micros-cl-provider
                #:plist-to-graph-node
                #:plist-to-graph-edge
                #:plist-to-call-graph))
(in-package #:lem-living-canvas/tests/micros-cl-provider)

;;; plist-to-graph-node tests

(deftest plist-to-graph-node-basic
  (testing "converts plist with all fields"
    (let* ((plist '(:id "MY-PKG:MY-FUNC"
                   :name "MY-FUNC"
                   :package "MY-PKG"
                   :type :function
                   :docstring "A test function"
                   :arglist "(x y)"
                   :source-file "/path/to/file.lisp"
                   :source-line 42))
           (node (plist-to-graph-node plist)))
      (ok (string= "MY-PKG:MY-FUNC" (graph-node-id node)))
      (ok (string= "MY-FUNC" (graph-node-name node)))
      (ok (string= "MY-PKG" (graph-node-package node)))
      (ok (eq :function (graph-node-type node)))
      (ok (string= "A test function" (graph-node-docstring node)))
      (ok (string= "(x y)" (graph-node-arglist node)))
      (ok (string= "/path/to/file.lisp" (graph-node-source-file node)))
      (ok (equal '("/path/to/file.lisp" . 42) (graph-node-source-location node))))))

(deftest plist-to-graph-node-minimal
  (testing "converts plist with minimal fields"
    (let* ((plist '(:id "PKG:FUNC" :name "FUNC" :package "PKG" :type :function))
           (node (plist-to-graph-node plist)))
      (ok (string= "PKG:FUNC" (graph-node-id node)))
      (ok (string= "FUNC" (graph-node-name node)))
      (ok (string= "PKG" (graph-node-package node)))
      (ok (eq :function (graph-node-type node)))
      (ok (null (graph-node-docstring node)))
      (ok (null (graph-node-arglist node)))
      (ok (null (graph-node-source-file node)))
      (ok (null (graph-node-source-location node))))))

(deftest plist-to-graph-node-types
  (testing "handles different node types"
    (dolist (type '(:function :macro :generic-function :command :major-mode :minor-mode))
      (let* ((plist (list :id "PKG:X" :name "X" :package "PKG" :type type))
             (node (plist-to-graph-node plist)))
        (ok (eq type (graph-node-type node))
            (format nil "type ~A should be preserved" type))))))

(deftest plist-to-graph-node-no-source-line
  (testing "handles source-file without source-line"
    (let* ((plist '(:id "PKG:F" :name "F" :package "PKG" :type :function
                   :source-file "/path/file.lisp"))
           (node (plist-to-graph-node plist)))
      (ok (string= "/path/file.lisp" (graph-node-source-file node)))
      ;; source-location should be (file . nil) when no line
      (ok (equal '("/path/file.lisp" . nil) (graph-node-source-location node))))))

;;; plist-to-graph-edge tests

(deftest plist-to-graph-edge-basic
  (testing "converts edge plist"
    (let* ((plist '(:source "PKG:CALLER" :target "PKG:CALLEE" :call-type :direct))
           (edge (plist-to-graph-edge plist)))
      (ok (string= "PKG:CALLER" (graph-edge-source edge)))
      (ok (string= "PKG:CALLEE" (graph-edge-target edge)))
      (ok (eq :direct (graph-edge-call-type edge))))))

(deftest plist-to-graph-edge-default-call-type
  (testing "defaults to :direct call-type when not specified"
    (let* ((plist '(:source "A" :target "B"))
           (edge (plist-to-graph-edge plist)))
      (ok (eq :direct (graph-edge-call-type edge))))))

(deftest plist-to-graph-edge-indirect
  (testing "handles indirect call type"
    (let* ((plist '(:source "X" :target "Y" :call-type :indirect))
           (edge (plist-to-graph-edge plist)))
      (ok (eq :indirect (graph-edge-call-type edge))))))

;;; plist-to-call-graph tests

(deftest plist-to-call-graph-empty
  (testing "handles empty plist"
    (let ((graph (plist-to-call-graph '(:nodes nil :edges nil))))
      (ok (call-graph-p graph))
      (ok (zerop (hash-table-count (call-graph-nodes graph))))
      (ok (null (call-graph-edges graph))))))

(deftest plist-to-call-graph-nodes-only
  (testing "converts graph with nodes only"
    (let* ((plist '(:nodes ((:id "PKG:A" :name "A" :package "PKG" :type :function)
                            (:id "PKG:B" :name "B" :package "PKG" :type :function))
                   :edges nil))
           (graph (plist-to-call-graph plist)))
      (ok (= 2 (hash-table-count (call-graph-nodes graph))))
      (ok (find-node graph "PKG:A"))
      (ok (find-node graph "PKG:B"))
      (ok (null (call-graph-edges graph))))))

(deftest plist-to-call-graph-complete
  (testing "converts complete graph with nodes and edges"
    (let* ((plist '(:nodes ((:id "PKG:MAIN" :name "MAIN" :package "PKG"
                             :type :function :docstring "Entry point"
                             :source-file "/src/main.lisp" :source-line 10)
                            (:id "PKG:HELPER" :name "HELPER" :package "PKG"
                             :type :function
                             :source-file "/src/main.lisp" :source-line 20))
                   :edges ((:source "PKG:MAIN" :target "PKG:HELPER"))))
           (graph (plist-to-call-graph plist)))
      ;; Check nodes
      (ok (= 2 (hash-table-count (call-graph-nodes graph))))
      (let ((main-node (find-node graph "PKG:MAIN")))
        (ok main-node)
        (ok (string= "MAIN" (graph-node-name main-node)))
        (ok (string= "Entry point" (graph-node-docstring main-node))))
      ;; Check edges
      (ok (= 1 (length (call-graph-edges graph))))
      (let ((edge (first (call-graph-edges graph))))
        (ok (string= "PKG:MAIN" (graph-edge-source edge)))
        (ok (string= "PKG:HELPER" (graph-edge-target edge)))))))

(deftest plist-to-call-graph-multiple-edges
  (testing "handles multiple edges"
    (let* ((plist '(:nodes ((:id "A" :name "A" :package "P" :type :function)
                            (:id "B" :name "B" :package "P" :type :function)
                            (:id "C" :name "C" :package "P" :type :function))
                   :edges ((:source "A" :target "B")
                           (:source "A" :target "C")
                           (:source "B" :target "C"))))
           (graph (plist-to-call-graph plist)))
      (ok (= 3 (hash-table-count (call-graph-nodes graph))))
      (ok (= 3 (length (call-graph-edges graph)))))))

(deftest plist-to-call-graph-nil-input
  (testing "handles nil plist gracefully"
    (let ((graph (plist-to-call-graph nil)))
      (ok (call-graph-p graph))
      (ok (zerop (hash-table-count (call-graph-nodes graph)))))))

;;; Integration test simulating micros response

(deftest integration-micros-response
  (testing "converts realistic micros response"
    (let* ((micros-response
             '(:nodes ((:id "LEM:FIND-FILE"
                        :name "FIND-FILE"
                        :package "LEM"
                        :type :command
                        :docstring "Open a file for editing"
                        :arglist "(filename)"
                        :source-file "/home/user/lem/src/commands.lisp"
                        :source-line 100)
                       (:id "LEM:FIND-FILE-BUFFER"
                        :name "FIND-FILE-BUFFER"
                        :package "LEM"
                        :type :function
                        :arglist "(filename)"
                        :source-file "/home/user/lem/src/commands.lisp"
                        :source-line 80)
                       (:id "LEM:WITH-OPEN-FILE-BUFFER"
                        :name "WITH-OPEN-FILE-BUFFER"
                        :package "LEM"
                        :type :macro
                        :arglist "((buffer filename) &body body)"
                        :source-file "/home/user/lem/src/macros.lisp"
                        :source-line 50))
               :edges ((:source "LEM:FIND-FILE" :target "LEM:FIND-FILE-BUFFER")
                       (:source "LEM:FIND-FILE" :target "LEM:WITH-OPEN-FILE-BUFFER"))))
           (graph (plist-to-call-graph micros-response)))
      ;; Verify structure
      (ok (= 3 (hash-table-count (call-graph-nodes graph))))
      (ok (= 2 (length (call-graph-edges graph))))
      ;; Verify command node
      (let ((cmd (find-node graph "LEM:FIND-FILE")))
        (ok cmd)
        (ok (eq :command (graph-node-type cmd)))
        (ok (string= "Open a file for editing" (graph-node-docstring cmd))))
      ;; Verify function node
      (let ((func (find-node graph "LEM:FIND-FILE-BUFFER")))
        (ok func)
        (ok (eq :function (graph-node-type func))))
      ;; Verify macro node
      (let ((macro (find-node graph "LEM:WITH-OPEN-FILE-BUFFER")))
        (ok macro)
        (ok (eq :macro (graph-node-type macro))))
      ;; Verify JSON conversion works
      (ok (hash-table-p (graph-to-cytoscape-json graph))))))
