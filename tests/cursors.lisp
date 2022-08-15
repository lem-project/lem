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

(defun make-testing-fake-cursors (point n)
  (lem:with-point ((p point))
    (loop :repeat n
          :do (assert (not (null (lem:line-offset p 1))))
              (lem::make-fake-cursor p))))

(test "Test to execute a series of commands"
  (with-testing-buffer (buffer (make-text-buffer (lines "abcdefg" "hijklmn" "opqrstu")))
    (make-testing-fake-cursors (lem:buffer-point buffer) 2)
    (test "execute self-insert command"
      (lem:execute-key-sequence (list (lem:make-key :sym " ")))
      (ok (string= (lines " abcdefg" " hijklmn" " opqrstu")
                   (lem:buffer-text buffer))))
    (test "execute delete-previous-character command"
      (lem:execute-key-sequence (list (lem:make-key :ctrl t :sym "h")))
      (lem:buffer-text buffer)
      (ok (string= (lines "abcdefg" "hijklmn" "opqrstu")
                   (lem:buffer-text buffer))))))
