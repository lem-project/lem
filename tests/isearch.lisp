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
  (lem-tests/buffer-list-test::with-buffer-list ()
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
                 (buffer-text buffer))))
    ;; XXX: After running this test, the buffer state will be broken
    #+(or)
    (progn
      (print (buffer-list)) ; => (#<BUFFER *tmp* NIL>)
      (let ((b (first (buffer-list))))
        ;; ???
        (assert (point< (buffer-point b)
                        (buffer-end-point b)))))
    ))
