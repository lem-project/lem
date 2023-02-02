(defpackage :lem-tests/history
  (:use :cl
        :rove
        :lem/common/history))
(in-package :lem-tests/history)

(deftest simple-test
  (let ((history (make-history)))
    (ok (null (last-history history)))
    (add-history history "foo")
    (ok (equal "foo" (last-history history)))
    (add-history history "bar")
    (ok (equal "bar" (last-history history)))
    (testing "previous-history"
      (ok (equal '("bar" t) (multiple-value-list (previous-history history))))
      (ok (equal '("foo" t) (multiple-value-list (previous-history history))))
      (ok (null (previous-history history))))
    (testing "next-history"
      (ok (equal '("bar" t) (multiple-value-list (next-history history))))
      (ok (null (next-history history))))))
