(defpackage :lem-lsp-mode/tests/json-lsp-utils
  (:use :cl
        :rove
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

(deftest coerce-json
  (let ((object (coerce-json (hash "line" 10
                                   "character" 3)
                             'position/test)))
    (ok (position-equals object :line 10 :character 3)))
  (let ((object (coerce-json (hash "start" (hash "line" 3 "character" 0)
                                   "end" (hash "line" 5 "character" 10))
                             'range/test)))
    (ok (typep object 'range/test))
    (ok (position-equals (slot-value object 'start) :line 3 :character 0))
    (ok (position-equals (slot-value object 'end) :line 5 :character 10))))

(deftest coerce-element
  (testing "lsp-array"
    (ok (signals (coerce-element 100 '(lem-lsp-mode/type:ts-array integer))
                 'json-type-error))
    (ok (signals (coerce-element '(1 "a") '(lem-lsp-mode/type:ts-array integer))
                 'json-type-error))
    (ok (equal '(1 2 3)
               (coerce-element '(1 2 3) '(lem-lsp-mode/type:ts-array integer))))
    (let ((result
            (coerce-element (list (hash "line" 10
                                        "character" 3)
                                  (hash "line" 3
                                        "character" 2)
                                  (hash "line" 0
                                        "character" 100))
                            '(lem-lsp-mode/type:ts-array position/test))))
      (ok (= 3 (length result)))
      (ok (position-equals (first result) :line 10 :character 3))
      (ok (position-equals (second result) :line 3 :character 2))
      (ok (position-equals (third result) :line 0 :character 100))))
  (testing "equal-specializer"
    (ok (signals (coerce-element 1 '(lem-lsp-mode/type:ts-equal-specializer "foo"))
                 'json-type-error))
    (ok (equal "foo" (coerce-element "foo" '(lem-lsp-mode/type:ts-equal-specializer "foo")))))
  (testing "object"
    (ok (signals (coerce-element 1
                                 '(lem-lsp-mode/type:ts-object string integer))
                 'json-type-error))
    (ok (signals (coerce-element (hash "foo" 100 'bar 200)
                                 '(lem-lsp-mode/type:ts-object string integer))
                 'json-type-error))
    (ok (hash-equal (coerce-element (hash "foo" 100 "bar" 200)
                                    '(lem-lsp-mode/type:ts-object string integer))
                    (hash "foo" 100 "bar" 200)))
    (ok (contain-hash-keys-p (coerce-element (hash "foo" '(100 200) "bar" '(1 2 3))
                                             '(lem-lsp-mode/type:ts-object string (lem-lsp-mode/type:ts-array integer)))
                             '("foo" "bar")))
    (ok (signals (coerce-element (hash "foo" '(100 200) "bar" '(1 "a" 3))
                                 '(lem-lsp-mode/type:ts-object string (lem-lsp-mode/type:ts-array integer)))
                 'json-type-error)))
  (testing "tuple"
    (ok (signals (coerce-element "foo" '(lem-lsp-mode/type:ts-tuple integer))
                 'json-type-error))
    (ok (signals (coerce-element '(1 2) '(lem-lsp-mode/type:ts-tuple integer))
                 'json-type-error))
    (ok (signals (coerce-element '(1 2) '(lem-lsp-mode/type:ts-tuple integer string))
                 'json-type-error))
    (ok (equal (coerce-element '(1 2) '(lem-lsp-mode/type:ts-tuple integer integer))
               '(1 2)))
    (ok (signals (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:ts-tuple string integer string))
                 'json-type-error))
    (ok (equal (coerce-element '(1 2 "foo") '(lem-lsp-mode/type:ts-tuple integer integer string))
               '(1 2 "foo"))))
  (testing "or"
    (ok (equal 1 (coerce-element 1 '(or integer string))))
    (ok (equal "a" (coerce-element "a" '(or integer string))))
    (ok (position-equals (coerce-element (hash "line" 10
                                               "character" 3)
                                         '(or position/test null))
                         :line 10
                         :character 3))
    (ok (position-equals (coerce-element (hash "line" 10
                                               "character" 3)
                                         '(or null position/test))
                         :line 10
                         :character 3)))
  (testing "interface"
    (let ((result (coerce-element
                   (hash "name" "abc"
                         "version" "1.0")
                   '(lem-lsp-mode/type:ts-interface
                     ("name" :type common-lisp:string)
                     ("version" :type common-lisp:string)))))
      (ok (hash-table-p result))
      (ok (equal "abc" (gethash "name" result)))
      (ok (equal "1.0" (gethash "version" result))))
    (let ((result (coerce-element (hash "foo" 100)
                                  `(lem-lsp-mode/type:ts-interface
                                    ("foo" :type integer :optional-p t)
                                    ("bar" :type string :optional-p t)))))
      (ok (hash-table-p result))
      (ok (equal 100 (gethash "foo" result)))))
  (testing "primitive"
    (ok (equal 1 (coerce-element 1 'integer)))
    (ok (equal nil (coerce-element nil 'boolean)))
    (ok (equal t (coerce-element t 'boolean)))))
