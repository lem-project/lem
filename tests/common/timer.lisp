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

(defvar *current-time* 0)

(defmethod lem/common/timer::get-microsecond-time ((timer-manager testing-timer-manager))
  *current-time*)

(defmacro with-testing-timer-manager (() &body body)
  `(with-timer-manager (make-instance 'testing-timer-manager)
     (let ((*current-time* 0)
           (lem/common/timer::*idle-timer-list* '()))
       ,@body)))

(deftest simple-timer-test
  (with-testing-timer-manager ()
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

(deftest compute-the-time-for-the-next-idle-timer-to-be-called
  (with-testing-timer-manager ()
    (ok (null (get-next-timer-timing-ms))))
  (with-testing-timer-manager ()
    (let ((timer1 (make-idle-timer (lambda ()) :name "idle-timer-1"))
          (timer2 (make-idle-timer (lambda ()) :name "idle-timer-2")))
      (start-timer timer1 10)
      (start-timer timer2 20)
      (with-idle-timers ()
        (ok (= 10 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 2)
        (ok (= 8 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 12)
        (ok (= 0 (get-next-timer-timing-ms))))
      (with-idle-timers ()
        (setf *current-time* 30)
        (ok (= -8 (get-next-timer-timing-ms)))))))
