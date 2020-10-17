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

(rove:deftest check-required-initarg
  (rove:testing "Missing parameters"
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
        (rove:ok (= 2 (length conditions)))
        (rove:ok (equals (first conditions) 'test-params 'a))
        (rove:ok (equals (second conditions) 'test-params 'c))))))

(rove:deftest primitive-value
  (rove:testing "st-json"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json)))
      (rove:ok (eq :null (json-null)))
      (rove:ok (eq :true (json-true)))
      (rove:ok (eq :false (json-false)))))
  (rove:testing "yason"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason)))
      (rove:ok (eq :null (json-null)))
      (rove:ok (eq t (json-true)))
      (rove:ok (eq nil (json-false))))))

(rove:deftest to-json
  (let ((test-params
          (make-instance 'test-params
                         :a "test"
                         :b 100
                         :c '(1 2))))
    (rove:testing "st-json"
      (let* ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json))
             (json (to-json test-params)))
        (rove:ok (typep json 'st-json:jso))
        (rove:ok (equal (st-json:getjso "a" json) "test"))
        (rove:ok (equal (st-json:getjso "b" json) 100))
        (rove:ok (equal (st-json:getjso "c" json) '(1 2)))))
    (rove:testing "yason"
      (let* ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason))
             (json (to-json test-params)))
        (rove:ok (hash-table-p json))
        (rove:ok (= 3 (hash-table-count json)))
        (rove:ok (equal "test" (gethash "a" json)))
        (rove:ok (equal 100 (gethash "b" json)))
        (rove:ok (equal '(1 2) (gethash "c" json)))))))

(rove:deftest json-get
  (rove:testing "st-json"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::st-json)))
      (rove:ok (equal 1
                      (json-get (st-json:jso "foo" 1 "bar" 2)
                                "foo")))
      (rove:ok (equal nil
                      (json-get (st-json:jso "foo" 1 "bar" 2)
                                "xxx")))))
  (rove:testing "yason"
    (let ((lem-lsp-mode/json::*json-library* (make-instance 'lem-lsp-mode/json::yason)))
      (rove:ok (equal 1
                      (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                                "foo")))
      (rove:ok (equal nil
                      (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                                "xxx"))))))
