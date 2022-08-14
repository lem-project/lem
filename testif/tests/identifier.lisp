(in-package :testif/tests)

(defun identifier-test ()
  (let* ((temp-package-name (gentemp))
         (temp-package (make-package temp-package-name))
         (id1 (make-instance 'testif::identifier
                             :name "foo"
                             :package temp-package))
         (id2 (make-instance 'testif::identifier
                             :name "bar"
                             :package temp-package))
         (test1 (make-instance 'testif::test :id id1 :function (lambda ())))
         (test1+ (make-instance 'testif::test :id id1 :function (lambda ())))
         (test2 (make-instance 'testif::test :id id2 :function (lambda ()))))
    (testif::register-test test1)
    (assert (eq test1 (testif::find-test id1)))
    (assert (set-equal (list test1)
                       (testif::package-tests temp-package)))

    (testif::register-test test1)
    (assert (eq test1 (testif::find-test id1)))
    (assert (set-equal (list test1) (testif::package-tests temp-package)))

    (testif::register-test test1+)
    (assert (eq test1+ (testif::find-test id1)))
    (assert (set-equal (list test1+) (testif::package-tests temp-package)))

    (testif::register-test test2)
    (assert (eq test2 (testif::find-test id2)))
    (assert (set-equal (list test1+ test2) (testif::package-tests temp-package)))

    (testif::remove-package-tests temp-package)))
