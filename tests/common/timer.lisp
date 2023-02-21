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

(defclass testing-timer-manager (timer-manager) ())

(defmethod send-timer-notification ((timer-manager testing-timer-manager)
                                    continue)
  (funcall continue))

(deftest simple-timer-test
  (let ((*timer-manager* (make-instance 'testing-timer-manager)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (start-timer (make-timer (lambda ()
                                 (sb-concurrency:send-message mailbox t)))
                   2
                   nil)
      (ok (sb-concurrency:receive-message mailbox))
      (let ((timeout (not (nth-value 1 (sb-concurrency:receive-message mailbox :timeout 0.01)))))
        (ok timeout)))
    (let* ((mailbox (sb-concurrency:make-mailbox))
           (timer (make-timer (lambda ()
                                (sb-concurrency:send-message mailbox t)))))
      (start-timer timer 2 t)
      (loop :repeat 2
            :do (multiple-value-bind (value received)
                    (sb-concurrency:receive-message mailbox :timeout 0.01)
                  (unless received (return))
                  (assert (eq value t))))
      (stop-timer timer)
      (ok (not (nth-value 1 (sb-concurrency:receive-message mailbox :timeout 0.01))))
      (ok (timer-expired-p timer)))))
