(defpackage :lem-tests/killring
  (:use :cl
        :testif)
  (:local-nicknames (:killring :lem/common/killring)))
(in-package :lem-tests/killring)

(test simple-test
  (let ((killring (killring:make-killring 10)))
    (ok (killring:push-item killring "abc"))
    (ok (equal "abc" (killring:peek-item killring 0)))
    (ok (equal "abc" (killring:peek-item killring 1)))
    (ok (killring:push-item killring "def"))
    (ok (equal "def" (killring:peek-item killring 0)))
    (ok (equal "abc" (killring:peek-item killring 1)))
    (ok (equal "def" (killring:peek-item killring 2)))
    (test "appending"
      (killring:with-context (:appending t)
        (killring:push-item killring "!!"))
      (ok (equal "def!!" (killring:peek-item killring 0))))
    (test "before-inserting"
      (killring:with-context (:appending t :before-inserting t)
        (killring:push-item killring "??"))
      (ok (equal "??def!!" (killring:peek-item killring 0))))))

(test before-inserting
  (let ((killring (killring:make-killring 10)))
    (killring:push-item killring "a")
    (killring:with-context (:appending t :before-inserting t)
      (killring:push-item killring "b"))
    (ok (equal "ba" (killring:peek-item killring 0)))
    (killring:with-context (:before-inserting t)
      (killring:with-context (:appending t)
        (killring:push-item killring "c")))
    (ok (equal "cba" (killring:peek-item killring 0)))))

(test rotate-to-empty-killring
  (let ((killring (killring:make-killring 10)))
    (killring:rotate killring)
    (killring:rotate-undo killring)
    (pass "no error")))

(test rotate
  (let ((killring (killring:make-killring 10)))
    (killring:push-item killring "a")
    (killring:push-item killring "b")
    (killring:rotate killring)
    (ok (equal "a" (killring:peek-item killring 0)))
    (ok (equal "b" (killring:peek-item killring 1)))
    (killring:rotate-undo killring)
    (ok (equal "b" (killring:peek-item killring 0)))
    (ok (equal "a" (killring:peek-item killring 1)))))

(test option
  (let ((killring (killring:make-killring 10)))
    (killring:push-item killring "foo" :options :test)
    (ok (equal '("foo" (:test))
               (multiple-value-list (killring:peek-item killring 0))))

    (killring:with-context (:appending t)
      (killring:push-item killring "bar" :options :test2))
    (ok (equal '("foobar" (:test :test2))
               (multiple-value-list (killring:peek-item killring 0))))

    (killring:with-context (:appending t :before-inserting t)
      (killring:push-item killring "baz" :options :test3))
    (ok (equal '("bazfoobar" (:test3 :test :test2))
               (multiple-value-list (killring:peek-item killring 0))))))

(test peek-item-when-empty
  (let ((killring (killring:make-killring 10)))
    (ok (null (killring:peek-item killring 0)))))
