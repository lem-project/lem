(defpackage #:lem-living-canvas/tests/python-provider
  (:use #:cl #:rove)
  (:import-from #:call-graph
                #:call-graph
                #:call-graph-nodes
                #:call-graph-edges
                #:make-call-graph
                #:graph-node-id
                #:graph-node-name
                #:graph-node-type
                #:graph-node-source-location
                #:graph-node-source-file
                #:graph-edge-source
                #:graph-edge-target))
(in-package #:lem-living-canvas/tests/python-provider)

;;; Check if Python provider can actually be initialized (grammar is available)

(defvar *provider-initialized*
  (and (find-package :tree-sitter)
       (ignore-errors
         (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :tree-sitter)))
       (ignore-errors
         (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
           (slot-value provider 'lem-living-canvas/python::language))))
  "True if Python tree-sitter provider initialized successfully (grammar available).")

;;; Test data - Python source code samples

(defparameter *simple-python-code*
  "def hello():
    print('Hello, world!')

def greet(name):
    return f'Hello, {name}!'
"
  "Simple Python code with two function definitions.")

(defparameter *python-with-calls*
  "def helper():
    return 42

def main():
    result = helper()
    print(result)
    return result
"
  "Python code with function calls between functions.")

(defparameter *python-class-code*
  "class Calculator:
    def __init__(self, value=0):
        self.value = value

    def add(self, x):
        self.value += x
        return self

    def multiply(self, x):
        self.value *= x
        return self

def use_calculator():
    calc = Calculator(10)
    calc.add(5).multiply(2)
"
  "Python code with class definition and method calls.")

(defparameter *python-nested-code*
  "def outer():
    def inner():
        return 1
    return inner()

def main():
    result = outer()
"
  "Python code with nested function definition.")

;;; T037: Test provider-supports-p with Python buffer

(deftest provider-supports-p-python-pathname
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-supports-p returns T for .py files"
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (ok (call-graph:provider-supports-p provider #P"test.py"))
          (ok (call-graph:provider-supports-p provider #P"/path/to/script.py"))
          (ok (call-graph:provider-supports-p provider #P"module.pyw"))))))

;;; T038: Test provider-supports-p rejects non-Python files

(deftest provider-supports-p-rejects-non-python
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-supports-p returns NIL for non-Python files"
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (ok (null (call-graph:provider-supports-p provider #P"test.js")))
          (ok (null (call-graph:provider-supports-p provider #P"main.lisp")))
          (ok (null (call-graph:provider-supports-p provider #P"script.ts")))))))

;;; T039: Test provider-analyze extracting function definitions

(deftest provider-analyze-extracts-functions
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze creates nodes for function definitions"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider *simple-python-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (ok (= 2 (hash-table-count nodes)) "Should find 2 functions")
          ;; Check that hello and greet functions are found
          (let ((node-names (loop :for node :being :the :hash-values :of nodes
                                  :collect (graph-node-name node))))
            (ok (member "hello" node-names :test #'string=))
            (ok (member "greet" node-names :test #'string=)))))))

;;; T040: Test provider-analyze extracting call edges

(deftest provider-analyze-extracts-calls
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze creates edges for function calls"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider *python-with-calls*
                                                   :type :string))
               (edges (call-graph-edges graph)))
          (ok (>= (length edges) 1) "Should find at least 1 call edge")
          ;; Check that main calls helper
          (let ((edge (find-if (lambda (e)
                                 (and (search "main" (graph-edge-source e))
                                      (search "helper" (graph-edge-target e))))
                               edges)))
            (ok edge "Should find edge from main to helper"))))))

;;; T041: Test source-location population in graph nodes

(deftest provider-analyze-populates-source-locations
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "nodes have source location information"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider *simple-python-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (maphash (lambda (id node)
                     (declare (ignore id))
                     (let ((loc (graph-node-source-location node)))
                       (ok loc (format nil "Node ~A should have source location"
                                       (graph-node-name node)))
                       (when loc
                         (ok (integerp (cdr loc))
                             "Source line should be an integer"))))
                   nodes)))))

;;; Additional tests for class/method support

(deftest provider-analyze-class-methods
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles class methods"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider *python-class-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (ok (>= (hash-table-count nodes) 3)
              "Should find at least use_calculator plus class methods")))))

;;; Test node types

(deftest provider-analyze-node-types
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "nodes have correct type (function)"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider *simple-python-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (maphash (lambda (id node)
                     (declare (ignore id))
                     (ok (member (graph-node-type node) '(:function :method :class))
                         (format nil "Node ~A should have valid type"
                                 (graph-node-name node))))
                   nodes)))))

;;; Test provider protocol methods

(deftest provider-name-returns-keyword
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-name returns :tree-sitter-python"
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (ok (eq :tree-sitter-python (call-graph:provider-name provider)))))))

(deftest provider-priority-returns-number
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-priority returns a positive number"
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (ok (plusp (call-graph:provider-priority provider)))))))

(deftest provider-languages-returns-python
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-languages includes :python"
        (let ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider)))
          (ok (member :python (call-graph:provider-languages provider)))))))

;;; Edge case tests

(deftest provider-analyze-empty-code
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles empty code"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               (graph (call-graph:provider-analyze provider "" :type :string)))
          (ok (call-graph:call-graph-p graph))
          (ok (zerop (hash-table-count (call-graph-nodes graph))))))))

(deftest provider-analyze-syntax-error
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles code with syntax errors gracefully"
        (let* ((provider (make-instance 'lem-living-canvas/python:tree-sitter-python-provider))
               ;; tree-sitter is error-tolerant, should still parse what it can
               (graph (call-graph:provider-analyze provider
                                                   "def broken(
    pass
def valid():
    pass"
                                                   :type :string)))
          (ok (call-graph:call-graph-p graph))
          ;; Should at least find the valid function
          (ok (>= (hash-table-count (call-graph-nodes graph)) 1))))))
