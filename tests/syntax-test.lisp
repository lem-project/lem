(defpackage :lem-tests/syntax-test
  (:use :cl)
  (:import-from :lem-tests/utilities
                :sample-file)
  (:import-from :lem)
  (:import-from :rove))
(in-package :lem-tests/syntax-test)

(rove:deftest case-1
  (let* ((buffer (lem:find-file-buffer (sample-file "syntax-sample.lisp")
                                       :temporary t
                                       :enable-undo-p nil
                                       :syntax-table lem-lisp-syntax:*syntax-table*))
         (point (lem:buffer-point buffer)))
    (lem:with-point ((point point))
      (lem:buffer-start point)
      (lem:form-offset point 1)
      (lem:form-offset point -1)
      (rove:ok (lem:start-buffer-p point)))
    (lem:with-point ((point point))
      (lem:buffer-start point)
      (lem:line-end point)
      (lem:form-offset point 1)
      (rove:ok (equal (lem:symbol-string-at-point point) "bar")))))
