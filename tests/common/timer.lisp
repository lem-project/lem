(defpackage :lem-tests/timer
  (:use :cl
        :rove
        :lem/common/timer))
(in-package :lem-tests/timer)

(defun testing-timer ()
  )

(deftest timer-name-tests
  (let* ((timer (make-timer 'testing-timer))
         (value (timer-name timer)))
    (ok (string= "TESTING-TIMER" value)))
  (let* ((timer (make-timer #'testing-timer))
         (value (timer-name timer)))
    (ok (string= "TESTING-TIMER" value)))
  (let* ((timer (make-timer (sb-int:named-lambda hello ())))
         (value (timer-name timer)))
    (ok (string= "HELLO" value)))
  (let* ((timer (make-timer 'testing-timer :name "foo"))
         (value (timer-name timer)))
    (ok (string= "foo" value))))
