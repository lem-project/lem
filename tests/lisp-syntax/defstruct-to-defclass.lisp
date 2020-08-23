(defpackage :lem-tests/lisp-syntax/defstruct-to-defclass
  (:use :cl)
  (:import-from :lem-base)
  (:import-from :lem-tests/utilities
                :sample-file)
  (:import-from :lem-lisp-syntax.defstruct-to-defclass
                :defstruct-to-defclass
                :analyze-defstruct
                :make-struct-info
                :struct-info-p
                :struct-start-point
                :struct-end-point
                :struct-name
                :struct-name-and-options-point
                :struct-slot-descriptions
                :slot-description-info-p
                :slot-description-complex-p
                :slot-description-name
                :slot-description-point
                :slot-description-initial-value-start-point
                :slot-description-initial-value-end-point
                :slot-description-read-only-p
                :slot-description-type-start-point
                :slot-description-type-end-point
                :translate-to-defclass-with-info)
  (:import-from :rove))
(in-package :lem-tests/lisp-syntax/defstruct-to-defclass)

(defun expected-point-position-p (point line-number charpos)
  (and (= line-number (lem-base:line-number-at-point point))
       (= charpos (lem-base:point-charpos point))))

(defun form-string-at-point (point)
  (lem-base:with-point ((start point)
                        (end point))
    (loop :while (lem-base:scan-lists start -1 1 t))
    (loop :until (lem-base:blank-line-p end) :do (lem-base:line-offset end 1))
    (lem-base:line-end end)
    (lem-base:points-to-string start end)))

(defun search-input-defstruct (point n)
  (lem-base:buffer-start point)
  (lem-base:search-forward point ";;; input")
  (loop :repeat n
        :do (lem-base:search-forward point "(defstruct"))
  (lem-base:scan-lists point -1 1 t))

(defun fetch-expected-form-string (buffer n)
  (lem-base:with-point ((point (lem-base:buffer-point buffer)))
    (lem-base:buffer-start point)
    (lem-base:search-forward point ";;; output")
    (loop :repeat n
          :do (lem-base:search-forward point "(defclass"))
    (form-string-at-point point)))

(defun make-test-buffer ()
  (let ((buffer (lem-base:find-file-buffer (sample-file "defstruct-to-defclass.lisp")
                                           :temporary t
                                           :syntax-table lem-lisp-syntax:*syntax-table*)))
    (setf (lem-base:variable-value 'lem-base:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    buffer))

(rove:deftest analyze-defstruct
  (rove:testing "simple"
    (let* ((buffer (make-test-buffer))
           (point (lem-base:buffer-point buffer)))
      (search-input-defstruct point 1)
      (let ((info (analyze-defstruct point (make-struct-info))))
        (rove:ok (struct-info-p info))
        (rove:ok (equal "foo" (struct-name info)))
        (rove:ok (expected-point-position-p (struct-start-point info) 3 1))
        (rove:ok (expected-point-position-p (struct-end-point info) 6 8))
        (rove:ok (expected-point-position-p (struct-name-and-options-point info) 3 11))
        (let ((slots (struct-slot-descriptions info)))
          (rove:ok (= (length slots) 3))
          (let ((slot (first slots)))
            (rove:ok (slot-description-info-p slot))
            (rove:ok (not (slot-description-complex-p slot)))
            (rove:ok (equal (slot-description-name slot) "slot-a"))
            (rove:ok (expected-point-position-p (slot-description-point slot) 4 2)))
          (let ((slot (second slots)))
            (rove:ok (slot-description-info-p slot))
            (rove:ok (not (slot-description-complex-p slot)))
            (rove:ok (equal (slot-description-name slot) "slot-b"))
            (rove:ok (expected-point-position-p (slot-description-point slot) 5 2)))
          (let ((slot (third slots)))
            (rove:ok (slot-description-info-p slot))
            (rove:ok (not (slot-description-complex-p slot)))
            (rove:ok (equal (slot-description-name slot) "slot-c"))
            (rove:ok (expected-point-position-p (slot-description-point slot) 6 2)))))))
  (rove:testing "complex slot-description"
    (let* ((buffer (make-test-buffer))
           (point (lem-base:buffer-point buffer)))
      (search-input-defstruct point 3)
      (let ((info (analyze-defstruct point (make-struct-info))))
        (rove:ok (struct-info-p info))
        (rove:ok (equal "foo" (struct-name info)))
        (let ((slots (struct-slot-descriptions info)))
          (rove:ok (= (length slots) 11))
          (flet ((test (slot
                        &key expected-slot-name
                             expected-point-line-number
                             expected-point-charpos
                             (expected-initform nil expected-initform-p)
                             expected-type
                             expected-read-only-p)
                   (rove:ok (slot-description-info-p slot))
                   (rove:ok (slot-description-complex-p slot))
                   (rove:ok (equal (slot-description-name slot) expected-slot-name))
                   (rove:ok (expected-point-position-p (slot-description-point slot)
                                                       expected-point-line-number
                                                       expected-point-charpos))
                   (if expected-initform-p
                       (rove:ok (equal expected-initform
                                       (read-from-string
                                        (lem-base:points-to-string (slot-description-initial-value-start-point slot)
                                                                   (slot-description-initial-value-end-point slot)))))
                       (rove:ok (and (null (slot-description-initial-value-start-point slot))
                                     (null (slot-description-initial-value-end-point slot)))))
                   (if (null expected-type)
                       (rove:ok (and (null (slot-description-type-start-point slot))
                                     (null (slot-description-type-end-point slot))))
                       (rove:ok (equal expected-type
                                       (read-from-string
                                        (lem-base:points-to-string (slot-description-type-start-point slot)
                                                                   (slot-description-type-end-point slot))))))
                   (if expected-read-only-p
                       (rove:ok (eq t (slot-description-read-only-p slot)))
                       (rove:ok (not (slot-description-read-only-p slot))))))
            (rove:testing "a"
              (test (elt slots 0)
                    :expected-slot-name "a"
                    :expected-point-line-number 17
                    :expected-point-charpos 3
                    :expected-initform 12
                    :expected-type nil
                    :expected-read-only-p nil))
            (rove:testing "b"
              (test (elt slots 1)
                    :expected-slot-name "b"
                    :expected-point-line-number 18
                    :expected-point-charpos 3
                    ;; :expected-initform nil
                    :expected-type nil
                    :expected-read-only-p nil))
            (rove:testing "c"
              (test (elt slots 2)
                    :expected-slot-name "c"
                    :expected-point-line-number 19
                    :expected-point-charpos 3
                    :expected-initform '(let ((x 0)) (f x))
                    :expected-type nil
                    :expected-read-only-p nil))
            (rove:testing "d"
              (test (elt slots 3)
                    :expected-slot-name "d"
                    :expected-point-line-number 21
                    :expected-point-charpos 3
                    :expected-initform 100
                    :expected-type 'integer
                    :expected-read-only-p nil))
            (rove:testing "e"
              (test (elt slots 4)
                    :expected-slot-name "e"
                    :expected-point-line-number 22
                    :expected-point-charpos 3
                    :expected-initform nil
                    :expected-type '(or nil string)
                    :expected-read-only-p nil))
            (rove:testing "f"
              (test (elt slots 5)
                    :expected-slot-name "f"
                    :expected-point-line-number 24
                    :expected-point-charpos 3
                    :expected-initform '(progn (foo))
                    :expected-type 'symbol
                    :expected-read-only-p nil))
            (rove:testing "g"
              (test (elt slots 6)
                    :expected-slot-name "g"
                    :expected-point-line-number 27
                    :expected-point-charpos 3
                    :expected-initform nil
                    :expected-type nil
                    :expected-read-only-p t))
            (rove:testing "h"
              (test (elt slots 7)
                    :expected-slot-name "h"
                    :expected-point-line-number 28
                    :expected-point-charpos 3
                    :expected-initform nil
                    :expected-type nil
                    :expected-read-only-p nil))
            (rove:testing "i"
              (test (elt slots 8)
                    :expected-slot-name "i"
                    :expected-point-line-number 29
                    :expected-point-charpos 3
                    :expected-initform nil
                    :expected-type nil
                    :expected-read-only-p t))
            (rove:testing "j"
              (test (elt slots 9)
                    :expected-slot-name "j"
                    :expected-point-line-number 30
                    :expected-point-charpos 3
                    :expected-initform 1
                    :expected-type 'integer
                    :expected-read-only-p t))
            (rove:testing "k"
              (test (elt slots 10)
                    :expected-slot-name "k"
                    :expected-point-line-number 33
                    :expected-point-charpos 3
                    :expected-initform 2
                    :expected-type 'integer
                    :expected-read-only-p t))))))))

(rove:deftest defstruct-to-defclass
  (flet ((test (n)
           (rove:testing (format nil "case-~D" n)
             (let* ((buffer (make-test-buffer))
                    (expected-form-string (fetch-expected-form-string buffer n))
                    (point (lem-base:buffer-point buffer)))
               (search-input-defstruct point n)
               (defstruct-to-defclass point)
               (rove:ok (equal (form-string-at-point point)
                               expected-form-string))))))
    (test 1)
    (test 2)
    (test 3)))
