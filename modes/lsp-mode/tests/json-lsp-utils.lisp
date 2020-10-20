(defpackage :lem-lsp-mode/tests/json-lsp-utils
  (:import-from :rove)
  (:use :cl
        :lem-lsp-mode/json
        :lem-lsp-mode/json-lsp-utils))
(in-package :lem-lsp-mode/tests/json-lsp-utils)

(defclass position/test (object)
  ((line :initarg :line :type number)
   (character :initarg :character :type number)))

(defclass range/test (object)
  ((start :initarg :start :type position/test)
   (end :initarg :end :type position/test)))

(defun hash-equal (ht1 ht2)
  (and (= (hash-table-count ht1)
          (hash-table-count ht2))
       (let ((default '#:default))
         (maphash (lambda (k v)
                    (unless (equal v (gethash k ht2 default))
                      (return-from hash-equal nil)))
                  ht1)
         t)))

(defun contain-hash-keys-p (ht keys)
  (alexandria:set-equal (alexandria:hash-table-keys ht)
                        keys
                        :test #'equal))

(defun hash (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun position-equals (object &key line character)
  (and (typep object 'position/test)
       (= line (slot-value object 'line))
       (= character (slot-value object 'character))))

(rove:deftest coerce-json
  (let ((object (coerce-json (hash "line" 10
                                   "character" 3)
                             'position/test)))
    (rove:ok (position-equals object :line 10 :character 3)))
  (let ((object (coerce-json (hash "start" (hash "line" 3 "character" 0)
                                   "end" (hash "line" 5 "character" 10))
                             'range/test)))
    (rove:ok (typep object 'range/test))
    (rove:ok (position-equals (slot-value object 'start) :line 3 :character 0))
    (rove:ok (position-equals (slot-value object 'end) :line 5 :character 10))))

(rove:deftest coerce-element
  (rove:testing "lsp-array"
    (rove:ok (rove:signals (coerce-element 100 '(lem-lsp-mode/type:lsp-array integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 "a") '(lem-lsp-mode/type:lsp-array integer))
                           'json-type-error))
    (rove:ok (equal '(1 2 3)
                    (coerce-element '(1 2 3) '(lem-lsp-mode/type:lsp-array integer))))
    (let ((result
            (coerce-element (list (hash "line" 10
                                        "character" 3)
                                  (hash "line" 3
                                        "character" 2)
                                  (hash "line" 0
                                        "character" 100))
                            '(lem-lsp-mode/type:lsp-array position/test))))
      (rove:ok (= 3 (length result)))
      (rove:ok (position-equals (first result) :line 10 :character 3))
      (rove:ok (position-equals (second result) :line 3 :character 2))
      (rove:ok (position-equals (third result) :line 0 :character 100))))
  (rove:testing "equal-specializer"
    (rove:ok (rove:signals (coerce-element 1 '(lem-lsp-mode/type:equal-specializer "foo"))
                           'json-type-error))
    (rove:ok (equal "foo" (coerce-element "foo" '(lem-lsp-mode/type:equal-specializer "foo")))))
  (rove:testing "object"
    (rove:ok (rove:signals (coerce-element 1
                                           '(lem-lsp-mode/type:object string integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element (hash "foo" 100 'bar 200)
                                           '(lem-lsp-mode/type:object string integer))
                           'json-type-error))
    (rove:ok (hash-equal (coerce-element (hash "foo" 100 "bar" 200)
                                         '(lem-lsp-mode/type:object string integer))
                         (hash "foo" 100 "bar" 200)))
    (rove:ok (contain-hash-keys-p (coerce-element (hash "foo" '(100 200) "bar" '(1 2 3))
                                                  '(lem-lsp-mode/type:object string (lem-lsp-mode/type:lsp-array integer)))
                                  '("foo" "bar")))
    (rove:ok (rove:signals (coerce-element (hash "foo" '(100 200) "bar" '(1 "a" 3))
                                           '(lem-lsp-mode/type:object string (lem-lsp-mode/type:lsp-array integer)))
                           'json-type-error)))
  (rove:testing "tuple"
    (rove:ok (rove:signals (coerce-element "foo" '(lem-lsp-mode/type:tuple integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer))
                           'json-type-error))
    (rove:ok (rove:signals (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer string))
                           'json-type-error))
    (rove:ok (equal (coerce-element '(1 2) '(lem-lsp-mode/type:tuple integer integer))
                    '(1 2)))
    (rove:ok (rove:signals (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:tuple string integer string))
                           'json-type-error))
    (rove:ok (equal (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:tuple integer integer string))
                    '(1 2 "foo"))))
  (rove:testing "or"
    (rove:ok (equal 1 (coerce-element 1 '(or integer string))))
    (rove:ok (equal "a" (coerce-element "a" '(or integer string))))
    (rove:ok (position-equals (coerce-element (hash "line" 10
                                                    "character" 3)
                                              '(or position/test null))
                              :line 10
                              :character 3))
    (rove:ok (position-equals (coerce-element (hash "line" 10
                                                    "character" 3)
                                              '(or null position/test))
                              :line 10
                              :character 3)))
  (rove:testing "interface"
    (let ((result (coerce-element
                   (hash "name" "abc"
                         "version" "1.0")
                   '(LEM-LSP-MODE/TYPE:INTERFACE
                     ("name" :TYPE COMMON-LISP:STRING)
                     ("version" :TYPE COMMON-LISP:STRING)))))
      (rove:ok (hash-table-p result))
      (rove:ok (equal "abc" (gethash "name" result)))
      (rove:ok (equal "1.0" (gethash "version" result))))))
