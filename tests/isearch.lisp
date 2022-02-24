(defpackage :lem-tests/isearch
  (:use :cl :rove :lem))
(in-package :lem-tests/isearch)

(defparameter *text* "
abcdefg
foo
foo
foo
xyz1234
")

(defun setup-testing-current-buffer (text)
  (let ((buffer (make-buffer "*isearch test*")))
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    (setf (current-buffer) buffer)))

(deftest replace-string
  (lem-tests/buffer-list-test::with-buffer-list ()
    (setup-testing-current-buffer *text*)
    (lem.isearch::query-replace-internal "foo"
                                         "foobar"
                                         #'search-forward
                                         #'search-backward
                                         :query nil
                                         :start (buffer-start-point (current-buffer))
                                         :end (buffer-end-point (current-buffer))
                                         :count 100)
    (ok (equal (ppcre:regex-replace-all "foo" *text* "foobar")
               (buffer-text (current-buffer))))))
