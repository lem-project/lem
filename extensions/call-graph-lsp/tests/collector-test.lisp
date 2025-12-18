(defpackage #:call-graph-lsp/tests/collector
  (:use #:cl #:rove)
  (:local-nicknames (:lsp :lem-lsp-base/protocol-3-17)))
(in-package #:call-graph-lsp/tests/collector)

;;; Test fixtures

(defun make-test-range (start-line start-char end-line end-char)
  "Create a test LSP Range object."
  (make-instance 'lsp:range
                 :start (make-instance 'lsp:position
                                       :line start-line
                                       :character start-char)
                 :end (make-instance 'lsp:position
                                     :line end-line
                                     :character end-char)))

(defun make-test-call-hierarchy-item (&key
                                        (name "testFunction")
                                        (kind lsp:symbol-kind-function)
                                        (uri "file:///path/to/test.go")
                                        (detail "(x int) string")
                                        (line 10)
                                        (char 0))
  "Create a test CallHierarchyItem object."
  (make-instance 'lsp:call-hierarchy-item
                 :name name
                 :kind kind
                 :uri uri
                 :detail detail
                 :range (make-test-range line char (+ line 5) 1)
                 :selection-range (make-test-range line char line (+ char (length name)))))

(defun make-test-incoming-call (from-name from-uri from-line)
  "Create a test CallHierarchyIncomingCall object."
  (make-instance 'lsp:call-hierarchy-incoming-call
                 :from (make-test-call-hierarchy-item
                        :name from-name
                        :uri from-uri
                        :line from-line)
                 :from-ranges (vector (make-test-range 15 4 15 20))))

(defun make-test-outgoing-call (to-name to-uri to-line)
  "Create a test CallHierarchyOutgoingCall object."
  (make-instance 'lsp:call-hierarchy-outgoing-call
                 :to (make-test-call-hierarchy-item
                      :name to-name
                      :uri to-uri
                      :line to-line)
                 :from-ranges (vector (make-test-range 12 8 12 24))))

;;; Mock callback factory

(defun make-mock-incoming-calls-fn (call-map)
  "Create a mock incoming calls function from a hash table mapping item names to lists of (from-name uri line)."
  (lambda (item)
    (let ((calls (gethash (lsp:call-hierarchy-item-name item) call-map)))
      (when calls
        (mapcar (lambda (call-spec)
                  (destructuring-bind (from-name from-uri from-line) call-spec
                    (make-test-incoming-call from-name from-uri from-line)))
                calls)))))

(defun make-mock-outgoing-calls-fn (call-map)
  "Create a mock outgoing calls function from a hash table mapping item names to lists of (to-name uri line)."
  (lambda (item)
    (let ((calls (gethash (lsp:call-hierarchy-item-name item) call-map)))
      (when calls
        (mapcar (lambda (call-spec)
                  (destructuring-bind (to-name to-uri to-line) call-spec
                    (make-test-outgoing-call to-name to-uri to-line)))
                calls)))))

;;; build-call-graph-from-hierarchy tests

(deftest build-call-graph-empty-items
  (testing "returns empty graph for empty items list"
    (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                  '()
                  (constantly nil)
                  (constantly nil))))
      (ok (hash-table-p (call-graph:call-graph-nodes graph)))
      (ok (zerop (hash-table-count (call-graph:call-graph-nodes graph))))
      (ok (null (call-graph:call-graph-edges graph))))))

(deftest build-call-graph-single-item-no-calls
  (testing "creates graph with single node when no calls exist"
    (let* ((item (make-test-call-hierarchy-item :name "standalone"))
           (graph (call-graph-lsp:build-call-graph-from-hierarchy
                   (list item)
                   (constantly nil)
                   (constantly nil))))
      (ok (= 1 (hash-table-count (call-graph:call-graph-nodes graph))))
      (ok (null (call-graph:call-graph-edges graph))))))

(deftest build-call-graph-with-outgoing-calls
  (testing "creates edges for outgoing calls"
    (let* ((main-item (make-test-call-hierarchy-item
                       :name "main"
                       :uri "file:///main.go"
                       :line 10))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; main calls helper and init
      (setf (gethash "main" outgoing-map)
            '(("helper" "file:///main.go" 30)
              ("init" "file:///main.go" 5)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list main-item)
                    (constantly nil)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-outgoing t)))
        ;; Should have 3 nodes: main, helper, init
        (ok (= 3 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 2 edges: main->helper, main->init
        (ok (= 2 (length (call-graph:call-graph-edges graph))))))))

(deftest build-call-graph-with-incoming-calls
  (testing "creates edges for incoming calls"
    (let* ((target-item (make-test-call-hierarchy-item
                         :name "handler"
                         :uri "file:///server.go"
                         :line 50))
           (incoming-map (make-hash-table :test 'equal)))
      ;; handler is called by router and middleware
      (setf (gethash "handler" incoming-map)
            '(("router" "file:///router.go" 100)
              ("middleware" "file:///middleware.go" 25)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list target-item)
                    (make-mock-incoming-calls-fn incoming-map)
                    (constantly nil)
                    :include-incoming t
                    :include-outgoing nil)))
        ;; Should have 3 nodes: handler, router, middleware
        (ok (= 3 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 2 edges: router->handler, middleware->handler
        (ok (= 2 (length (call-graph:call-graph-edges graph))))))))

(deftest build-call-graph-both-directions
  (testing "creates edges for both incoming and outgoing calls"
    (let* ((middle-item (make-test-call-hierarchy-item
                         :name "process"
                         :uri "file:///process.go"
                         :line 20))
           (incoming-map (make-hash-table :test 'equal))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; process is called by main
      (setf (gethash "process" incoming-map)
            '(("main" "file:///main.go" 10)))
      ;; process calls helper
      (setf (gethash "process" outgoing-map)
            '(("helper" "file:///helper.go" 5)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list middle-item)
                    (make-mock-incoming-calls-fn incoming-map)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-incoming t
                    :include-outgoing t)))
        ;; Should have 3 nodes: main, process, helper
        (ok (= 3 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 2 edges: main->process, process->helper
        (ok (= 2 (length (call-graph:call-graph-edges graph))))))))

(deftest build-call-graph-deduplicates-nodes
  (testing "does not duplicate nodes seen multiple times"
    (let* ((item1 (make-test-call-hierarchy-item
                   :name "func1"
                   :uri "file:///main.go"
                   :line 10))
           (item2 (make-test-call-hierarchy-item
                   :name "func2"
                   :uri "file:///main.go"
                   :line 20))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; Both func1 and func2 call shared
      (setf (gethash "func1" outgoing-map)
            '(("shared" "file:///shared.go" 5)))
      (setf (gethash "func2" outgoing-map)
            '(("shared" "file:///shared.go" 5)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list item1 item2)
                    (constantly nil)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-outgoing t)))
        ;; Should have 3 nodes: func1, func2, shared (not duplicated)
        (ok (= 3 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 2 edges
        (ok (= 2 (length (call-graph:call-graph-edges graph))))))))

(deftest build-call-graph-deduplicates-edges
  (testing "does not duplicate edges with same source and target"
    (let* ((item (make-test-call-hierarchy-item
                  :name "caller"
                  :uri "file:///main.go"
                  :line 10))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; caller calls callee twice (different call sites, same function)
      (setf (gethash "caller" outgoing-map)
            '(("callee" "file:///callee.go" 5)
              ("callee" "file:///callee.go" 5)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list item)
                    (constantly nil)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-outgoing t)))
        ;; Should have 2 nodes: caller, callee
        (ok (= 2 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 1 edge (deduplicated)
        (ok (= 1 (length (call-graph:call-graph-edges graph))))))))

(deftest build-call-graph-respects-include-flags
  (testing "only processes requested direction"
    (let* ((item (make-test-call-hierarchy-item
                  :name "target"
                  :uri "file:///target.go"
                  :line 10))
           (incoming-map (make-hash-table :test 'equal))
           (outgoing-map (make-hash-table :test 'equal)))
      (setf (gethash "target" incoming-map)
            '(("caller" "file:///caller.go" 5)))
      (setf (gethash "target" outgoing-map)
            '(("callee" "file:///callee.go" 20)))
      ;; Only outgoing
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list item)
                    (make-mock-incoming-calls-fn incoming-map)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-incoming nil
                    :include-outgoing t)))
        (ok (= 2 (hash-table-count (call-graph:call-graph-nodes graph))))
        (ok (= 1 (length (call-graph:call-graph-edges graph))))
        ;; Edge should be target->callee
        (let ((edge (first (call-graph:call-graph-edges graph))))
          (ok (search "target" (call-graph:graph-edge-source edge)))
          (ok (search "callee" (call-graph:graph-edge-target edge)))))
      ;; Only incoming
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list item)
                    (make-mock-incoming-calls-fn incoming-map)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-incoming t
                    :include-outgoing nil)))
        (ok (= 2 (hash-table-count (call-graph:call-graph-nodes graph))))
        (ok (= 1 (length (call-graph:call-graph-edges graph))))
        ;; Edge should be caller->target
        (let ((edge (first (call-graph:call-graph-edges graph))))
          (ok (search "caller" (call-graph:graph-edge-source edge)))
          (ok (search "target" (call-graph:graph-edge-target edge))))))))

(deftest build-call-graph-calls-progress-fn
  (testing "calls progress function for each item"
    (let* ((items (list (make-test-call-hierarchy-item :name "func1" :line 10)
                        (make-test-call-hierarchy-item :name "func2" :line 20)
                        (make-test-call-hierarchy-item :name "func3" :line 30)))
           (progress-calls '()))
      (call-graph-lsp:build-call-graph-from-hierarchy
       items
       (constantly nil)
       (constantly nil)
       :progress-fn (lambda (current total)
                      (push (cons current total) progress-calls)))
      ;; Should have been called 3 times
      (ok (= 3 (length progress-calls)))
      ;; Check progression
      (let ((sorted (sort progress-calls #'< :key #'car)))
        (ok (equal '((1 . 3) (2 . 3) (3 . 3)) sorted))))))

(deftest build-call-graph-handles-nil-callback-return
  (testing "handles nil returned from callback functions"
    (let* ((item (make-test-call-hierarchy-item :name "lonely"))
           (graph (call-graph-lsp:build-call-graph-from-hierarchy
                   (list item)
                   (constantly nil)
                   (constantly nil)
                   :include-incoming t
                   :include-outgoing t)))
      (ok (= 1 (hash-table-count (call-graph:call-graph-nodes graph))))
      (ok (null (call-graph:call-graph-edges graph))))))

;;; Integration test

(deftest integration-complete-call-graph
  (testing "builds complete graph from multiple items with calls"
    (let* ((main-item (make-test-call-hierarchy-item
                       :name "main"
                       :uri "file:///main.go"
                       :line 10))
           (process-item (make-test-call-hierarchy-item
                          :name "process"
                          :uri "file:///process.go"
                          :line 20))
           (outgoing-map (make-hash-table :test 'equal)))
      ;; main calls process
      (setf (gethash "main" outgoing-map)
            '(("process" "file:///process.go" 20)))
      ;; process calls helper1 and helper2
      (setf (gethash "process" outgoing-map)
            '(("helper1" "file:///helpers.go" 5)
              ("helper2" "file:///helpers.go" 15)))
      (let ((graph (call-graph-lsp:build-call-graph-from-hierarchy
                    (list main-item process-item)
                    (constantly nil)
                    (make-mock-outgoing-calls-fn outgoing-map)
                    :include-outgoing t)))
        ;; Should have 4 nodes: main, process, helper1, helper2
        (ok (= 4 (hash-table-count (call-graph:call-graph-nodes graph))))
        ;; Should have 3 edges: main->process, process->helper1, process->helper2
        (ok (= 3 (length (call-graph:call-graph-edges graph))))
        ;; Verify JSON conversion works
        (let ((json (with-output-to-string (s)
                      (call-graph:call-graph-to-json graph s))))
          (ok (stringp json))
          (ok (search "main" json))
          (ok (search "process" json))
          (ok (search "helper1" json))
          (ok (search "helper2" json)))))))
