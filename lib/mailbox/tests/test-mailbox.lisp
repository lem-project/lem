(defpackage :lem-tests/lem-mailbox
  (:use :cl :rove :lem-mailbox))


(in-package :lem-tests/lem-mailbox)
;;TODO: Fix the prefix of mailbox for tests

(deftest mailbox-trivia.1
  (ok (lem-mailbox::mailboxp (lem-mailbox::make-mailbox)))
  (ok (not (lem-mailbox::mailboxp 42))))


(deftest mailbox-trivia.2
  (let ((mbox1 (lem-mailbox::make-mailbox :name "foof"))
	(mbox2 (lem-mailbox::make-mailbox)))
    (ok (string= "foof" (lem-mailbox::mailbox-name mbox1)))
    (ok (not (lem-mailbox::mailbox-name mbox2)))))


(deftest mailbox-trivia.3
  (flet ((test (initial-contents)
	   (let ((mbox (lem-mailbox::make-mailbox :initial-contents initial-contents)))
	     (list (lem-mailbox::mailbox-count mbox)
		   (lem-mailbox::mailbox-empty-p mbox)
		   (lem-mailbox::list-mailbox-messages mbox)
		   (eq (lem-mailbox::list-mailbox-messages mbox) initial-contents)))))
    (ok (equal '(3 nil (1 2 3) nil) (test '(1 2 3))))
    (ok (equal '(3 nil (1 2 3) nil) (test #(1 2 3))))
    (ok (equal '(3 nil (#\1 #\2 #\3) nil) (test "123")))
    (ok '(0 t nil t) (test nil))))


#+bordeaux-threads
(deftest mailbox-timeouts
  (let* ((mbox (lem-mailbox::make-mailbox))
	 (writers (loop for i from 1 upto 20
			collect (bt:make-thread
				 (lambda ()
				   (loop repeat 50
					 do (lem-mailbox::send-message mbox i)
					    (sleep 0))))))
	 (readers (loop repeat 10
			collect (bt:make-thread
				 (lambda ()
				   (loop while (lem-mailbox::receive-message mbox :timeout 0.5)
					 count t))))))
    (mapc #'bt:join-thread writers)
    (ok (= 1000
	   (apply #'+ (mapcar #'bt:join-thread readers))))))
