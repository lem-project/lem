(defpackage #:call-graph/tests/registry
  (:use #:cl #:rove #:call-graph))
(in-package #:call-graph/tests/registry)

;;; Test provider implementations for registry tests

(defclass mock-python-provider (call-graph-provider)
  ((priority-value :initarg :priority :initform 5 :accessor mock-provider-priority-value))
  (:documentation "Mock Python provider for testing"))

(defmethod provider-name ((provider mock-python-provider))
  :mock-python)

(defmethod provider-priority ((provider mock-python-provider))
  (mock-provider-priority-value provider))

(defmethod provider-supports-p ((provider mock-python-provider) source)
  (and (pathnamep source)
       (string-equal "py" (pathname-type source))))

(defmethod provider-analyze ((provider mock-python-provider) source &key &allow-other-keys)
  (declare (ignore source))
  (make-call-graph :root-package "mock-python"))

(defclass mock-js-provider (call-graph-provider)
  ((priority-value :initarg :priority :initform 5 :accessor mock-provider-priority-value))
  (:documentation "Mock JavaScript provider for testing"))

(defmethod provider-name ((provider mock-js-provider))
  :mock-javascript)

(defmethod provider-priority ((provider mock-js-provider))
  (mock-provider-priority-value provider))

(defmethod provider-supports-p ((provider mock-js-provider) source)
  (and (pathnamep source)
       (member (pathname-type source) '("js" "ts") :test #'string-equal)))

(defmethod provider-analyze ((provider mock-js-provider) source &key &allow-other-keys)
  (declare (ignore source))
  (make-call-graph :root-package "mock-javascript"))

(defclass mock-lsp-python-provider (call-graph-provider)
  ()
  (:documentation "Mock high-priority LSP Python provider for testing"))

(defmethod provider-name ((provider mock-lsp-python-provider))
  :mock-lsp-python)

(defmethod provider-priority ((provider mock-lsp-python-provider))
  10)  ; Higher priority than tree-sitter

(defmethod provider-supports-p ((provider mock-lsp-python-provider) source)
  (and (pathnamep source)
       (string-equal "py" (pathname-type source))))

(defmethod provider-analyze ((provider mock-lsp-python-provider) source &key &allow-other-keys)
  (declare (ignore source))
  (make-call-graph :root-package "mock-lsp-python"))

;;; Helper to create fresh registry for each test

(defun make-test-registry ()
  "Create a fresh provider registry for testing."
  (make-instance 'call-graph:provider-registry))

;;; T006: Test provider-registry class existence

(deftest registry-class-exists
  (testing "provider-registry class is defined"
    (ok (find-class 'call-graph:provider-registry nil))))

(deftest registry-instantiable
  (testing "provider-registry can be instantiated"
    (ok (make-test-registry))))

;;; T007: Tests for register-provider

(deftest register-provider-basic
  (testing "register-provider adds provider to registry"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-python-provider)))
      (call-graph:register-provider registry provider '(:python))
      (ok (call-graph:find-provider registry :python)))))

(deftest register-provider-multiple-languages
  (testing "register-provider handles multiple languages"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-js-provider)))
      (call-graph:register-provider registry provider '(:javascript :typescript))
      (ok (call-graph:find-provider registry :javascript))
      (ok (call-graph:find-provider registry :typescript)))))

(deftest register-provider-returns-provider
  (testing "register-provider returns the registered provider"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-python-provider)))
      (let ((result (call-graph:register-provider registry provider '(:python))))
        (ok (eq result provider))))))

(deftest register-provider-overwrites-same-name
  (testing "registering provider with same name overwrites"
    (let ((registry (make-test-registry))
          (provider1 (make-instance 'mock-python-provider :priority 1))
          (provider2 (make-instance 'mock-python-provider :priority 99)))
      (call-graph:register-provider registry provider1 '(:python))
      (call-graph:register-provider registry provider2 '(:python))
      ;; Should have both in language list (not overwrite in language map)
      ;; But provider lookup by name should find the latest
      (let ((found (call-graph:find-provider registry :python)))
        ;; Should find the highest priority one
        (ok (= 99 (provider-priority found)))))))

;;; T008: Tests for find-provider with priority ordering

(deftest find-provider-returns-highest-priority
  (testing "find-provider returns highest priority provider"
    (let ((registry (make-test-registry))
          (low-priority (make-instance 'mock-python-provider :priority 3))
          (high-priority (make-instance 'mock-lsp-python-provider)))
      (call-graph:register-provider registry low-priority '(:python))
      (call-graph:register-provider registry high-priority '(:python))
      (let ((found (call-graph:find-provider registry :python)))
        (ok (eq :mock-lsp-python (provider-name found)))))))

(deftest find-provider-returns-nil-for-unknown-language
  (testing "find-provider returns nil for unknown language"
    (let ((registry (make-test-registry)))
      (ok (null (call-graph:find-provider registry :unknown-lang))))))

(deftest find-provider-with-source-filters-by-support
  (testing "find-provider with source checks provider-supports-p"
    (let ((registry (make-test-registry))
          (python-provider (make-instance 'mock-python-provider))
          (js-provider (make-instance 'mock-js-provider)))
      (call-graph:register-provider registry python-provider '(:python))
      (call-graph:register-provider registry js-provider '(:javascript))
      ;; Python source should select Python provider
      (let ((found (call-graph:find-provider registry :python #P"test.py")))
        (ok found)
        (ok (eq :mock-python (provider-name found))))
      ;; Non-matching source should return nil
      (let ((found (call-graph:find-provider registry :python #P"test.js")))
        (ok (null found))))))

(deftest find-provider-priority-with-source-filter
  (testing "find-provider with source returns highest priority that supports it"
    (let ((registry (make-test-registry))
          (low-priority (make-instance 'mock-python-provider :priority 5))
          (high-priority (make-instance 'mock-lsp-python-provider)))
      (call-graph:register-provider registry low-priority '(:python))
      (call-graph:register-provider registry high-priority '(:python))
      (let ((found (call-graph:find-provider registry :python #P"test.py")))
        ;; Both support .py, should get highest priority
        (ok (eq :mock-lsp-python (provider-name found)))))))

;;; T009: Tests for list-providers

(deftest list-providers-all
  (testing "list-providers returns all registered providers"
    (let ((registry (make-test-registry))
          (python-provider (make-instance 'mock-python-provider))
          (js-provider (make-instance 'mock-js-provider)))
      (call-graph:register-provider registry python-provider '(:python))
      (call-graph:register-provider registry js-provider '(:javascript))
      (let ((all-providers (call-graph:list-providers registry)))
        (ok (= 2 (length all-providers)))
        (ok (member python-provider all-providers))
        (ok (member js-provider all-providers))))))

(deftest list-providers-by-language
  (testing "list-providers filtered by language"
    (let ((registry (make-test-registry))
          (python-provider (make-instance 'mock-python-provider))
          (js-provider (make-instance 'mock-js-provider)))
      (call-graph:register-provider registry python-provider '(:python))
      (call-graph:register-provider registry js-provider '(:javascript))
      (let ((python-providers (call-graph:list-providers registry :python)))
        (ok (= 1 (length python-providers)))
        (ok (eq python-provider (first python-providers))))
      (let ((js-providers (call-graph:list-providers registry :javascript)))
        (ok (= 1 (length js-providers)))
        (ok (eq js-provider (first js-providers)))))))

(deftest list-providers-empty-registry
  (testing "list-providers on empty registry returns empty list"
    (let ((registry (make-test-registry)))
      (ok (null (call-graph:list-providers registry)))
      (ok (null (call-graph:list-providers registry :python))))))

(deftest list-providers-multiple-for-language
  (testing "list-providers returns multiple providers for same language"
    (let ((registry (make-test-registry))
          (ts-provider (make-instance 'mock-python-provider))
          (lsp-provider (make-instance 'mock-lsp-python-provider)))
      (call-graph:register-provider registry ts-provider '(:python))
      (call-graph:register-provider registry lsp-provider '(:python))
      (let ((providers (call-graph:list-providers registry :python)))
        (ok (= 2 (length providers)))))))

;;; Tests for unregister-provider

(deftest unregister-provider-removes-from-registry
  (testing "unregister-provider removes provider"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-python-provider)))
      (call-graph:register-provider registry provider '(:python))
      (ok (call-graph:find-provider registry :python))
      (call-graph:unregister-provider registry :mock-python)
      (ok (null (call-graph:find-provider registry :python))))))

(deftest unregister-provider-returns-boolean
  (testing "unregister-provider returns t if found, nil otherwise"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-python-provider)))
      (call-graph:register-provider registry provider '(:python))
      (ok (call-graph:unregister-provider registry :mock-python))
      (ok (null (call-graph:unregister-provider registry :mock-python)))
      (ok (null (call-graph:unregister-provider registry :nonexistent))))))

;;; Tests for *provider-registry* global instance

(deftest global-registry-exists
  (testing "*provider-registry* is defined and is a registry"
    (ok (boundp 'call-graph:*provider-registry*))
    (ok (typep call-graph:*provider-registry* 'call-graph:provider-registry))))

;;; Edge case tests

(deftest register-provider-empty-languages-list
  (testing "register-provider with empty languages list"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-python-provider)))
      ;; Should not error, but provider won't be findable by language
      (call-graph:register-provider registry provider '())
      ;; Provider is registered by name
      (ok (= 1 (length (call-graph:list-providers registry)))))))

(deftest multiple-registrations-same-provider
  (testing "same provider can be registered for multiple languages"
    (let ((registry (make-test-registry))
          (provider (make-instance 'mock-js-provider)))
      (call-graph:register-provider registry provider '(:javascript :ecmascript :typescript))
      (ok (eq provider (call-graph:find-provider registry :javascript)))
      (ok (eq provider (call-graph:find-provider registry :ecmascript)))
      (ok (eq provider (call-graph:find-provider registry :typescript))))))
