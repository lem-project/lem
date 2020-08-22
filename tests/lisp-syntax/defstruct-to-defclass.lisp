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
                :slot-description-info-name
                :slot-description-info-point
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
          (rove:ok (equal (slot-description-info-name slot) "slot-a"))
          (rove:ok (expected-point-position-p (slot-description-info-point slot) 4 2)))
        (let ((slot (second slots)))
          (rove:ok (slot-description-info-p slot))
          (rove:ok (equal (slot-description-info-name slot) "slot-b"))
          (rove:ok (expected-point-position-p (slot-description-info-point slot) 5 2)))
        (let ((slot (third slots)))
          (rove:ok (slot-description-info-p slot))
          (rove:ok (equal (slot-description-info-name slot) "slot-c"))
          (rove:ok (expected-point-position-p (slot-description-info-point slot) 6 2)))))))

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
