(defpackage :lem-lsp-mode/json-test
  (:use :cl
        :rove
        :lem-lsp-mode/json))
(in-package :lem-lsp-mode/json-test)

(defclass test-params (object)
  ((a
    :initarg :a)
   (b?
    :initarg :b)
   (c
    :initarg :c)))

(deftest check-required-initarg
  (testing "Missing parameters"
    (flet ((make ()
             (let ((conditions '()))
               (handler-bind ((missing-parameter
                                (lambda (c)
                                  (push c conditions)
                                  (continue c))))
                 (make-instance 'test-params))
               (nreverse conditions)))
           (equals (condition class-name slot-name)
             (and (eq (missing-parameter-class-name condition) class-name)
                  (eq (missing-parameter-slot-name condition) slot-name))))
      (let ((conditions (make)))
        (ok (= 2 (length conditions)))
        (ok (equals (first conditions) 'test-params 'a))
        (ok (equals (second conditions) 'test-params 'c))))))

(deftest primitive-value
  (testing "st-json"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json)))
      (ok (eq :null (json-null)))
      (ok (eq :true (json-true)))
      (ok (eq :false (json-false)))))
  (testing "yason"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason)))
      (ok (eq :null (json-null)))
      (ok (eq t (json-true)))
      (ok (eq nil (json-false))))))

(deftest to-json
  (let ((test-params
          (make-instance 'test-params
                         :a "test"
                         :b 100
                         :c '(1 2))))
    (testing "st-json"
      (let* ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json))
             (json (to-json test-params)))
        (ok (typep json 'st-json:jso))
        (ok (equal (st-json:getjso "a" json) "test"))
        (ok (equal (st-json:getjso "b" json) 100))
        (ok (equal (st-json:getjso "c" json) '(1 2)))))
    (testing "yason"
      (let* ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason))
             (json (to-json test-params)))
        (ok (hash-table-p json))
        (ok (= 3 (hash-table-count json)))
        (ok (equal "test" (gethash "a" json)))
        (ok (equal 100 (gethash "b" json)))
        (ok (equal '(1 2) (gethash "c" json)))))))

(deftest json-get
  (testing "st-json"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json)))
      (ok (equal 1
                 (json-get (st-json:jso "foo" 1 "bar" 2)
                           "foo")))
      (ok (equal nil
                 (json-get (st-json:jso "foo" 1 "bar" 2)
                           "xxx")))))
  (testing "yason"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason)))
      (ok (equal 1
                 (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                           "foo")))
      (ok (equal nil
                 (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                           "xxx"))))))
