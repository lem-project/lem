(defpackage :lem-tests/killring
  (:use :cl
        :testif
        :lem/common/killring))
(in-package :lem-tests/killring)

(test simple-test
  (let ((killring (make-killring 10)))
    (ok (push-killring-item killring "abc"))
    (ok (equal "abc" (peek-killring-item killring 0)))
    (ok (equal "abc" (peek-killring-item killring 1)))
    (ok (push-killring-item killring "def"))
    (ok (equal "def" (peek-killring-item killring 0)))
    (ok (equal "abc" (peek-killring-item killring 1)))
    (ok (equal "def" (peek-killring-item killring 2)))
    (test "appending"
      (with-killring-context (:appending t)
        (push-killring-item killring "!!"))
      (ok (equal "def!!" (peek-killring-item killring 0))))
    (test "before-inserting"
      (with-killring-context (:appending t :before-inserting t)
        (push-killring-item killring "??"))
      (ok (equal "??def!!" (peek-killring-item killring 0))))))

(test before-inserting
  (let ((killring (make-killring 10)))
    (push-killring-item killring "a")
    (with-killring-context (:appending t :before-inserting t)
      (push-killring-item killring "b"))
    (ok (equal "ba" (peek-killring-item killring 0)))
    (with-killring-context (:before-inserting t)
      (with-killring-context (:appending t)
        (push-killring-item killring "c")))
    (ok (equal "cba" (peek-killring-item killring 0)))))

(test rotate-to-empty-killring
  (let ((killring (make-killring 10)))
    (rotate-killring killring)
    (rotate-killring-undo killring)
    (pass "no error")))

(test rotate
  (let ((killring (make-killring 10)))
    (push-killring-item killring "a")
    (push-killring-item killring "b")
    (rotate-killring killring)
    (ok (equal "a" (peek-killring-item killring 0)))
    (ok (equal "b" (peek-killring-item killring 1)))
    (rotate-killring-undo killring)
    (ok (equal "b" (peek-killring-item killring 0)))
    (ok (equal "a" (peek-killring-item killring 1)))))

(test option
  (let ((killring (make-killring 10)))
    (push-killring-item killring "foo" :options :test)
    (ok (equal '("foo" (:test))
               (multiple-value-list (peek-killring-item killring 0))))

    (with-killring-context (:appending t)
      (push-killring-item killring "bar" :options :test2))
    (ok (equal '("foobar" (:test :test2))
               (multiple-value-list (peek-killring-item killring 0))))

    (with-killring-context (:appending t :before-inserting t)
      (push-killring-item killring "baz" :options :test3))
    (ok (equal '("bazfoobar" (:test3 :test :test2))
               (multiple-value-list (peek-killring-item killring 0))))))

(test peek-killring-item-when-empty
  (let ((killring (make-killring 10)))
    (ok (null (peek-killring-item killring 0)))))
