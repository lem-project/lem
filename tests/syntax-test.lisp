(defpackage :lem-tests/syntax-test
  (:use :cl :rove)
  (:import-from :lem-tests/utilities
                :sample-file)
  (:import-from :lem-lisp-mode)
  (:import-from :lem-base))
(in-package :lem-tests/syntax-test)

(deftest form-offset
  (let ((lem-lisp-mode::*disable-self-connect* t))
    (testing "skip comment"
      (let* ((buffer (lem-base:find-file-buffer (sample-file "syntax-sample.lisp")
                                                :temporary t
                                                :enable-undo-p nil
                                                :syntax-table lem-lisp-syntax:*syntax-table*))
             (point (lem-base:buffer-point buffer)))
        (lem-base:with-point ((point point))
          (lem-base:buffer-start point)
          (lem-base:form-offset point 1)
          (lem-base:form-offset point -1)
          (ok (lem-base:start-buffer-p point)))
        (lem-base:with-point ((point point))
          (lem-base:buffer-start point)
          (lem-base:line-end point)
          (lem-base:form-offset point 1)
          (ok (equal (lem-base:symbol-string-at-point point) "bar")))))))

(defparameter +scan-lists-sample-text+
  (string-trim '(#\space #\newline) "
\(a
 (b
  c)
 d)
"))

(deftest scan-lists
  (let ((lem-lisp-mode::*disable-self-connect* t))
    (testing "limit-point"
      (let* ((buffer (lem-base:make-buffer nil
                                           :temporary t
                                           :enable-undo-p nil
                                           :syntax-table lem-lisp-syntax:*syntax-table*))
             (point (lem-base:buffer-point buffer)))
        (lem-base:insert-string point +scan-lists-sample-text+)
        (lem-base:with-point ((point point)
                              (limit-point point))
          (testing "forward"
            (assert (lem-base:search-forward (lem-base:buffer-start limit-point) "c)"))
            (lem-base:buffer-start point)
            (ok (and (null (lem-base:scan-lists point 1 0 t limit-point))
                     (lem-base:start-buffer-p point)))
            (ok (and (eq point (lem-base:scan-lists point 1 0 t))
                     (= 4 (lem-base:line-number-at-point point))
                     (= 3 (lem-base:point-charpos point)))))
          (testing "backward"
            (lem-base:buffer-end point)
            (assert (lem-base:search-forward (lem-base:buffer-start limit-point) "(b"))
            (ok (and (null (lem-base:scan-lists point -1 0 t limit-point))
                     (lem-base:end-buffer-p point)))
            (ok (and (eq point (lem-base:scan-lists point -1 0 t))
                     (lem-base:start-buffer-p point)))))))))
