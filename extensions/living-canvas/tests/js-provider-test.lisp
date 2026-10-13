(defpackage #:lem-living-canvas/tests/js-provider
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
(in-package #:lem-living-canvas/tests/js-provider)

;;; Check if JavaScript provider can actually be initialized (grammar is available)

(defvar *provider-initialized*
  (and (find-package :tree-sitter)
       (ignore-errors
         (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :tree-sitter)))
       (ignore-errors
         (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
           (slot-value provider 'lem-js-mode/call-graph::language))))
  "True if JavaScript tree-sitter provider initialized successfully (grammar available).")

;;; Test data - JavaScript/TypeScript source code samples

(defparameter *simple-js-code*
  "function hello() {
    console.log('Hello, world!');
}

function greet(name) {
    return `Hello, ${name}!`;
}
"
  "Simple JavaScript code with two function declarations.")

(defparameter *js-with-calls*
  "function helper() {
    return 42;
}

function main() {
    const result = helper();
    console.log(result);
    return result;
}
"
  "JavaScript code with function calls between functions.")

(defparameter *js-arrow-functions*
  "const add = (a, b) => a + b;

const multiply = (a, b) => {
    return a * b;
};

const compute = (x, y) => {
    const sum = add(x, y);
    return multiply(sum, 2);
};
"
  "JavaScript code with arrow functions.")

(defparameter *js-class-code*
  "class Calculator {
    constructor(value = 0) {
        this.value = value;
    }

    add(x) {
        this.value += x;
        return this;
    }

    multiply(x) {
        this.value *= x;
        return this;
    }
}

function useCalculator() {
    const calc = new Calculator(10);
    calc.add(5).multiply(2);
}
"
  "JavaScript code with class definition and method calls.")

(defparameter *typescript-code*
  "interface User {
    name: string;
    age: number;
}

function createUser(name: string, age: number): User {
    return { name, age };
}

const getFullName = (user: User): string => {
    return user.name;
}

function processUser(name: string, age: number) {
    const user = createUser(name, age);
    return getFullName(user);
}
"
  "TypeScript code with interfaces and typed functions.")

;;; T061: Test provider-supports-p with JavaScript

(deftest provider-supports-p-javascript-pathname
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-supports-p returns T for .js files"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (call-graph:provider-supports-p provider #P"app.js"))
          (ok (call-graph:provider-supports-p provider #P"/path/to/script.mjs"))
          (ok (call-graph:provider-supports-p provider #P"component.jsx"))))))

;;; T062: Test provider-supports-p with TypeScript

(deftest provider-supports-p-typescript-pathname
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-supports-p returns T for .ts/.tsx files"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (call-graph:provider-supports-p provider #P"app.ts"))
          (ok (call-graph:provider-supports-p provider #P"component.tsx"))))))

(deftest provider-supports-p-rejects-non-js
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-supports-p returns NIL for non-JS files"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (null (call-graph:provider-supports-p provider #P"test.py")))
          (ok (null (call-graph:provider-supports-p provider #P"main.lisp")))))))

;;; T063: Test provider-analyze extracting function declarations

(deftest provider-analyze-extracts-function-declarations
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze creates nodes for function declarations"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *simple-js-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (ok (= 2 (hash-table-count nodes)) "Should find 2 functions")
          ;; Check that hello and greet functions are found
          (let ((node-names (loop :for node :being :the :hash-values :of nodes
                                  :collect (graph-node-name node))))
            (ok (member "hello" node-names :test #'string=))
            (ok (member "greet" node-names :test #'string=)))))))

;;; T064: Test provider-analyze extracting arrow functions

(deftest provider-analyze-extracts-arrow-functions
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze creates nodes for arrow functions"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *js-arrow-functions*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (ok (>= (hash-table-count nodes) 3) "Should find at least 3 arrow functions")
          (let ((node-names (loop :for node :being :the :hash-values :of nodes
                                  :collect (graph-node-name node))))
            (ok (member "add" node-names :test #'string=))
            (ok (member "multiply" node-names :test #'string=))
            (ok (member "compute" node-names :test #'string=)))))))

;;; T065: Test provider-analyze extracting method definitions

(deftest provider-analyze-extracts-methods
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles class methods"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *js-class-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          (ok (>= (hash-table-count nodes) 3)
              "Should find useCalculator plus class methods")))))

;;; Test call edge extraction

(deftest provider-analyze-extracts-calls
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze creates edges for function calls"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *js-with-calls*
                                                   :type :string))
               (edges (call-graph-edges graph)))
          (ok (>= (length edges) 1) "Should find at least 1 call edge")
          ;; Check that main calls helper
          (let ((edge (find-if (lambda (e)
                                 (and (search "main" (graph-edge-source e))
                                      (search "helper" (graph-edge-target e))))
                               edges)))
            (ok edge "Should find edge from main to helper"))))))

;;; Test source location population

(deftest provider-analyze-populates-source-locations
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "nodes have source location information"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *simple-js-code*
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

;;; Test TypeScript code parsing

(deftest provider-analyze-typescript
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles TypeScript code"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider *typescript-code*
                                                   :type :string))
               (nodes (call-graph-nodes graph)))
          ;; Should find createUser, getFullName, processUser
          (ok (>= (hash-table-count nodes) 3) "Should find at least 3 functions")))))

;;; Test provider protocol methods

(deftest provider-name-returns-keyword
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-name returns :tree-sitter-javascript"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (eq :tree-sitter-javascript (call-graph:provider-name provider)))))))

(deftest provider-priority-returns-number
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-priority returns a positive number"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (plusp (call-graph:provider-priority provider)))))))

(deftest provider-languages-returns-js-ts
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-languages includes :javascript and :typescript"
        (let ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider)))
          (ok (member :javascript (call-graph:provider-languages provider)))
          (ok (member :typescript (call-graph:provider-languages provider)))))))

;;; Edge case tests

(deftest provider-analyze-empty-code
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles empty code"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               (graph (call-graph:provider-analyze provider "" :type :string)))
          (ok (call-graph:call-graph-p graph))
          (ok (zerop (hash-table-count (call-graph-nodes graph))))))))

(deftest provider-analyze-syntax-error
  (if (not *provider-initialized*)
      (skip "Tree-sitter not available")
      (testing "provider-analyze handles code with syntax errors gracefully"
        (let* ((provider (make-instance 'lem-js-mode/call-graph:tree-sitter-js-provider))
               ;; tree-sitter is error-tolerant, should still parse what it can
               (graph (call-graph:provider-analyze provider
                                                   "function broken(
    return;
}
function valid() {
    return true;
}"
                                                   :type :string)))
          (ok (call-graph:call-graph-p graph))
          ;; Should at least find the valid function
          (ok (>= (hash-table-count (call-graph-nodes graph)) 1))))))
