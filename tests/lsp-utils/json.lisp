(defpackage :lem-tests/lsp-utils/json
  (:use :cl
        :rove
        :lem-lsp-utils/json)
  (:import-from :trivial-package-local-nicknames)
  ;; TODO
  (:import-from :lem-lsp-utils/protocol))
(in-package :lem-tests/lsp-utils/json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :protocol :lem-lsp-utils/protocol))

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
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (ok (eq :null (json-null)))
      (ok (eq :true (json-true)))
      (ok (eq :false (json-false)))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (ok (eq :null (json-null)))
      (ok (eq t (json-true)))
      (ok (eq 'yason:false (json-false))))))

(deftest json-array
  (testing "st-json"
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (ok (equal '(1 2 3)
                 (json-array 1 2 3)))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (ok (equalp #(1 2 3)
                  (json-array 1 2 3))))))

(deftest make-json
  (testing "st-json"
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (let ((json (make-json "foo" 100 "bar" 200 :foo-bar 300)))
        (ok (typep json 'st-json:jso))
        (ok (equal (json-get json "foo") 100))
        (ok (equal (json-get json "bar") 200))
        (ok (equal (json-get json "fooBar") 300)))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (let ((json (make-json "foo" 100 "bar" 200 :foo-bar 300)))
        (ok (hash-table-p json))
        (ok (equal (json-get json "foo") 100))
        (ok (equal (json-get json "bar") 200))
        (ok (equal (json-get json "fooBar") 300))
        json))))

(deftest object-to-json
  (let ((test-params
          (make-instance 'test-params
                         :a "test"
                         :b 100
                         :c '(1 2))))
    (testing "st-json"
      (let* ((*json-backend* (make-instance 'st-json-backend))
             (json (object-to-json test-params)))
        (ok (typep json 'st-json:jso))
        (ok (equal (st-json:getjso "a" json) "test"))
        (ok (equal (st-json:getjso "b" json) 100))
        (ok (equal (st-json:getjso "c" json) '(1 2)))))
    (testing "yason"
      (let* ((*json-backend* (make-instance 'yason-backend))
             (json (object-to-json test-params)))
        (ok (hash-table-p json))
        (ok (= 3 (hash-table-count json)))
        (ok (equal "test" (gethash "a" json)))
        (ok (equal 100 (gethash "b" json)))
        (ok (equal '(1 2) (gethash "c" json)))))))

(deftest object-to-json/nest-structure
  (let ((json
          (object-to-json
           (make-instance
            'protocol:hover-params
            :text-document (make-instance
                            'protocol:text-document-identifier
                            :uri "file:///foo/bar/test.txt")
            :position (make-instance 'protocol:position :character 3 :line 5)))))
    (ok (= 2 (json-object-length json)))
    (ok (= 1 (json-object-length (json-get json "textDocument"))))
    (ok (equal "file:///foo/bar/test.txt" (json-get (json-get json "textDocument") "uri")))
    (ok (= 2 (json-object-length (json-get json "position"))))
    (ok (= 5 (json-get (json-get json "position") "line")))
    (ok (= 3 (json-get (json-get json "position") "character"))))
  (let ((json
          (object-to-json
           (make-instance
            'protocol:did-change-text-document-params
            :text-document (make-instance 'protocol:versioned-text-document-identifier
                                          :uri "file://Users/user/test.lisp"
                                          :version 0)
            :content-changes (json-array
                              (make-json :range (make-instance 'protocol:range
                                                               :start (make-instance 'protocol:position :character 2 :line 1)
                                                               :end (make-instance 'protocol:position :character 3 :line 5))
                                         :rangeLength 0
                                         :text "abc"))))))
    (ok (equal "file://Users/user/test.lisp"
               (json-get* json "textDocument" "uri")))
    (ok (equal 0
               (json-get* json "textDocument" "version")))
    (ok (json-array-p (json-get json "contentChanges")))
    (let ((content-change (elt (json-get json "contentChanges") 0)))
      (ok (equal 1 (json-get* content-change "range" "start" "line")))
      (ok (equal 2 (json-get* content-change "range" "start" "character")))
      (ok (equal 5 (json-get* content-change "range" "end" "line")))
      (ok (equal 3 (json-get* content-change "range" "end" "character"))))))

(deftest json-get
  (testing "st-json"
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (ok (equal 1
                 (json-get (st-json:jso "foo" 1 "bar" 2)
                           "foo")))
      (ok (equal nil
                 (json-get (st-json:jso "foo" 1 "bar" 2)
                           "xxx")))
      (ok (eq :unbound
              (json-get (st-json:jso "foo" 1)
                        "xxx"
                        :unbound)))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (ok (equal 1
                 (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                           "foo")))
      (ok (equal nil
                 (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                           "xxx")))
      (ok (eq :unbound
              (json-get (alexandria:plist-hash-table (list "foo" 1 "bar" 2) :test 'equal)
                        "xxx"
                        :unbound))))))

(deftest json-array-p
  (testing "st-json"
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (ok (not (json-array-p 1)))
      (ok (not (json-array-p #(1 2 3))))
      (ok (not (json-array-p '(1 2 . 3))))
      (ok (json-array-p '()))
      (ok (json-array-p '(1 2 3)))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (ok (not (json-array-p 1)))
      (ok (json-array-p #(1 2 3)))
      (ok (not (json-array-p '(1 2 . 3))))
      (ok (not (json-array-p '())))
      (ok (not (json-array-p '(1 2 3)))))))

(deftest json-object-p
  (testing "st-json"
    (let ((*json-backend* (make-instance 'st-json-backend)))
      (ok (not (json-object-p 1)))
      (ok (not (json-object-p #())))
      (ok (not (json-object-p nil)))
      (ok (json-object-p (st-json:jso "foo" 1)))
      (ok (not (json-object-p (make-hash-table))))))
  (testing "yason"
    (let ((*json-backend* (make-instance 'yason-backend)))
      (ok (not (json-object-p 1)))
      (ok (not (json-object-p #())))
      (ok (not (json-object-p nil)))
      (ok (not (json-object-p (st-json:jso "foo" 1))))
      (ok (json-object-p (make-hash-table))))))
