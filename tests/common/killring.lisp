(defpackage :lem-tests/killring
  (:use :cl
        :rove
        :lem/common/killring
        :lem))
(in-package :lem-tests/killring)

(deftest simple-test
  (let ((killring (make-killring 10)))
    (ok (push-killring-item killring "abc"))
    (ok (equal "abc" (peek-killring-item killring 0)))
    (ok (equal "abc" (peek-killring-item killring 1)))
    (ok (push-killring-item killring "def"))
    (ok (equal "def" (peek-killring-item killring 0)))
    (ok (equal "abc" (peek-killring-item killring 1)))
    (ok (equal "def" (peek-killring-item killring 2)))
    (testing "appending"
      (with-killring-context (:appending t)
        (push-killring-item killring "!!"))
      (ok (equal "def!!" (peek-killring-item killring 0))))
    (testing "before-inserting"
      (with-killring-context (:appending t :before-inserting t)
        (push-killring-item killring "??"))
      (ok (equal "??def!!" (peek-killring-item killring 0))))))

(deftest appending-if-empty
  (let ((killring (make-killring 10)))
    (with-killring-context (:appending t)
      (push-killring-item killring "abc")
      (ok (equal "abc" (peek-killring-item killring 0))))))

(deftest before-inserting
  (let ((killring (make-killring 10)))
    (push-killring-item killring "a")
    (with-killring-context (:appending t :before-inserting t)
      (push-killring-item killring "b"))
    (ok (equal "ba" (peek-killring-item killring 0)))
    (with-killring-context (:before-inserting t)
      (with-killring-context (:appending t)
        (push-killring-item killring "c")))
    (ok (equal "cba" (peek-killring-item killring 0)))))

(deftest rotate-to-empty-killring
  (let ((killring (make-killring 10)))
    (rotate-killring killring)
    (rotate-killring-undo killring)
    (pass "no error")))

(deftest rotate
  (let ((killring (make-killring 10)))
    (push-killring-item killring "a")
    (push-killring-item killring "b")
    (rotate-killring killring)
    (ok (equal "a" (peek-killring-item killring 0)))
    (ok (equal "b" (peek-killring-item killring 1)))
    (rotate-killring-undo killring)
    (ok (equal "b" (peek-killring-item killring 0)))
    (ok (equal "a" (peek-killring-item killring 1)))))

(deftest internal-option
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

(deftest external-option
  (let ((killring (make-killring 2)))
    (with-killring-context (:appending t :before-inserting t)
      (push-killring-item killring "baz" :options :test))

    ;; clipboard disabled
    (let ((lem::*enable-clipboard-p* nil)
          (lem::*killring* killring))
      (ok (equal '("baz" (:test))
                 (multiple-value-list (yank-from-clipboard-or-killring)))))
    
    ;; clipboard enabled
    (let ((lem::*enable-clipboard-p* t)
          (lem::*killring* killring)
          (expected-result "In LEM we trust."))
      (lem::invoke-frontend (lambda ()))
      (copy-to-clipboard-with-killring expected-result)
      (ok (equal expected-result
                 (yank-from-clipboard-or-killring))))))

(deftest peek-killring-item-when-empty
  (let ((killring (make-killring 10)))
    (ok (null (peek-killring-item killring 0)))))
