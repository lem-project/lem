(defpackage :call-graph/tests/provider
  (:use :cl :rove :call-graph))
(in-package :call-graph/tests/provider)

;;; Test provider implementation for testing the protocol

(defclass test-provider (call-graph-provider)
  ((supported-types :initarg :supported-types
                    :initform '(:test)
                    :accessor test-provider-supported-types)
   (priority-value :initarg :priority
                   :initform 5
                   :accessor test-provider-priority-value))
  (:documentation "Test implementation of call-graph-provider"))

(defmethod provider-name ((provider test-provider))
  :test)

(defmethod provider-supports-p ((provider test-provider) source)
  (member (type-of source) (test-provider-supported-types provider)))

(defmethod provider-analyze ((provider test-provider) source &key)
  (let ((graph (make-call-graph)))
    (add-node graph (make-graph-node
                     :id "TEST:ANALYZED"
                     :name "ANALYZED"
                     :package "TEST"))
    graph))

(defmethod provider-priority ((provider test-provider))
  (test-provider-priority-value provider))

;;; call-graph-provider base class tests

(deftest call-graph-provider-is-class
  (testing "call-graph-provider is a class"
    (ok (find-class 'call-graph-provider))))

(deftest call-graph-provider-instantiable
  (testing "can instantiate call-graph-provider"
    (ok (make-instance 'call-graph-provider))))

;;; provider-priority tests

(deftest provider-priority-default
  (testing "default priority is 0"
    (let ((provider (make-instance 'call-graph-provider)))
      (ok (= 0 (provider-priority provider))))))

(deftest provider-priority-custom
  (testing "subclass can override priority"
    (let ((provider (make-instance 'test-provider :priority 10)))
      (ok (= 10 (provider-priority provider))))))

;;; provider-name tests

(deftest provider-name-returns-keyword
  (testing "provider-name returns a keyword"
    (let ((provider (make-instance 'test-provider)))
      (ok (keywordp (provider-name provider)))
      (ok (eq :test (provider-name provider))))))

;;; provider-supports-p tests

(deftest provider-supports-p-returns-boolean
  (testing "provider-supports-p returns generalized boolean"
    (let ((provider (make-instance 'test-provider :supported-types '(string))))
      (ok (provider-supports-p provider "test"))
      (ok (not (provider-supports-p provider 123))))))

(deftest provider-supports-p-multiple-types
  (testing "can support multiple types"
    (let ((provider (make-instance 'test-provider
                                   :supported-types '(string pathname))))
      (ok (provider-supports-p provider "test"))
      (ok (provider-supports-p provider #p"/path/to/file"))
      (ok (not (provider-supports-p provider 42))))))

;;; provider-analyze tests

(deftest provider-analyze-returns-call-graph
  (testing "provider-analyze returns a call-graph"
    (let* ((provider (make-instance 'test-provider))
           (result (provider-analyze provider :any-source)))
      (ok (call-graph-p result))
      (ok (= 1 (hash-table-count (call-graph-nodes result)))))))

(deftest provider-analyze-with-keyword-args
  (testing "provider-analyze accepts keyword arguments"
    (let ((provider (make-instance 'test-provider)))
      ;; Should not error with extra keyword args
      (ok (call-graph-p (provider-analyze provider :source :type :file))))))

;;; Protocol completeness tests

(deftest protocol-all-generics-defined
  (testing "all protocol generics are defined"
    (ok (fboundp 'provider-name))
    (ok (fboundp 'provider-supports-p))
    (ok (fboundp 'provider-analyze))
    (ok (fboundp 'provider-priority))))

;;; Multiple providers scenario

(defclass high-priority-provider (test-provider)
  ()
  (:default-initargs :priority 100))

(defmethod provider-name ((provider high-priority-provider))
  :high-priority)

(defclass low-priority-provider (test-provider)
  ()
  (:default-initargs :priority 1))

(defmethod provider-name ((provider low-priority-provider))
  :low-priority)

(deftest multiple-providers-priority-ordering
  (testing "providers can be sorted by priority"
    (let ((providers (list (make-instance 'low-priority-provider)
                           (make-instance 'high-priority-provider)
                           (make-instance 'test-provider :priority 50))))
      (let ((sorted (sort (copy-list providers) #'> :key #'provider-priority)))
        (ok (eq :high-priority (provider-name (first sorted))))
        (ok (eq :test (provider-name (second sorted))))
        (ok (eq :low-priority (provider-name (third sorted))))))))

(deftest provider-selection-by-support
  (testing "can select provider based on source support"
    (let ((string-provider (make-instance 'test-provider
                                          :supported-types '(string)
                                          :priority 10))
          (number-provider (make-instance 'test-provider
                                          :supported-types '(integer)
                                          :priority 5)))
      (let ((providers (list string-provider number-provider)))
        ;; Find provider for string
        (let ((found (find-if (lambda (p) (provider-supports-p p "hello"))
                              providers)))
          (ok (eq found string-provider)))
        ;; Find provider for integer
        (let ((found (find-if (lambda (p) (provider-supports-p p 42))
                              providers)))
          (ok (eq found number-provider)))))))

;;; Edge case tests

(deftest provider-analyze-empty-source
  (testing "provider can handle nil source"
    (let ((provider (make-instance 'test-provider)))
      ;; Should not error
      (ok (call-graph-p (provider-analyze provider nil))))))

(deftest provider-supports-p-nil-source
  (testing "provider-supports-p handles nil source"
    (let ((provider (make-instance 'test-provider :supported-types '(null))))
      (ok (provider-supports-p provider nil)))
    (let ((provider (make-instance 'test-provider :supported-types '(string))))
      (ok (not (provider-supports-p provider nil))))))
