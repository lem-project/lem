(defpackage :lem-tests/isearch
  (:use :cl :lem-tests/deftest :lem))
(in-package :lem-tests/isearch)

(defparameter *text* "
abcdefg
foo
foo
foo
xyz1234
")

(defun make-test-buffer (text)
  (let ((buffer (make-buffer "test" :temporary t)))
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(deftest replace-string
  (let ((buffer (make-test-buffer *text*)))
    (lem.isearch::query-replace-internal "foo"
                                         "foobar"
                                         #'search-forward
                                         #'search-backward
                                         :query nil
                                         :start (buffer-start-point buffer)
                                         :end (buffer-end-point buffer)
                                         :count 100)
    (ok (equal (ppcre:regex-replace-all "foo" *text* "foobar")
               (buffer-text buffer)))))
