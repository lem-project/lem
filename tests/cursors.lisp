(defpackage :lem-tests/cursors
  (:use :cl
        :testif
        :lem-tests/utilities)
  (:import-from :lem/common/killring
                :make-killring
                :push-killring-item
                :peek-killring-item))
(in-package :lem-tests/cursors)

(defun make-testing-killring ()
  (let ((killring (lem/common/killring:make-killring 10)))
    (push-killring-item killring "aaa")
    (push-killring-item killring "bbb")
    (push-killring-item killring "ccc")
    killring))

(test simple-fake-cursor-test
  (with-testing-buffer (buffer (lem:make-buffer "cursor test"))
    (let* ((lem::*killring* (make-testing-killring))
           (cursor (lem::make-fake-cursor (lem:current-point))))
      (test "Test the fake-cursor created"
        (test "buffer-fake-cursors"
          (ok (alexandria:length= 1 (lem::buffer-fake-cursors buffer)))
          (ok (eq cursor (first (lem::buffer-fake-cursors buffer)))))
        (test "killring"
          (let ((killring (lem::fake-cursor-killring cursor)))
            (ok (not (eq (lem::current-killring) killring)))
            (ok (equal "aaa" (peek-killring-item killring 2)))
            (ok (equal "bbb" (peek-killring-item killring 1)))
            (ok (equal "ccc" (peek-killring-item killring 0)))))
        (test "point-kind"
          (ok (eq :left-inserting (lem:point-kind cursor)))))
      (test "Delete cursor"
        (lem::delete-fake-cursor cursor)
        (ok (null (lem::buffer-fake-cursors buffer)))))))
