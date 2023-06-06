(in-package :lem-mailbox)


(defstruct (mailbox (:constructor %make-mailbox (queue semaphore name))
                    (:copier nil)
                    (:predicate mailboxp))
  "Mailbox aka message queue.

SEND-MESSAGE adds a message to the mailbox, RECEIVE-MESSAGE waits till
a message becomes available, whereas RECEIVE-MESSAGE-NO-HANG is a non-blocking
variant, and RECEIVE-PENDING-MESSAGES empties the entire mailbox in one go.

Messages can be arbitrary objects"
  (queue (missing-arg) :type queues::simple-cqueue)
  (semaphore (missing-arg) :type BT-SEMAPHORE::SEMAPHORE)
  (name nil))


(setf (documentation 'mailboxp 'function)
      "Returns true if argument is a MAILBOX, NIL otherwise."
      (documentation 'mailbox-name 'function)
      "Name of a MAILBOX. SETFable.")

(defun make-mailbox (&key name initial-contents)
  "Returns a new MAILBOX with messages in INITIAL-CONTENTS enqueued."
  (flet ((genname (thing name)
           (format nil "~:[Mailbox ~A~;~A for mailbox ~S~]" name thing name))
	 (init-queue (queue vec)
	   (loop :for x :across vec
		 :do (queues:qpush queue x)
		 :finally (return queue))))
    (let ((mailbox (%make-mailbox
		    (queues:make-queue :simple-cqueue)
		    (bt-semaphore:make-semaphore
		     :name (genname "Semaphore" name)
		     :count (length initial-contents))
		    name)))
      (when initial-contents
	(etypecase initial-contents
	  (vector
	   (setf (lem-mailbox::mailbox-queue mailbox)
		 (init-queue
		  (mailbox-queue mailbox)
		  initial-contents)))
	  (list
	   (setf (lem-mailbox::mailbox-queue mailbox)
		 (init-queue
		  (lem-mailbox::mailbox-queue mailbox)
		  (coerce initial-contents 'vector))))
	  (nil nil)
	  (atom
	   (setf (lem-mailbox::mailbox-queue mailbox)
		 (queues:qpush
		  (lem-mailbox::mailbox-queue mailbox)
		  initial-contents)))))
      mailbox)))

(defmethod print-object ((mailbox mailbox) stream)
  (print-unreadable-object (mailbox stream :type t :identity t)
    (format stream "~@[~S ~](~D msgs pending)"
            (mailbox-name mailbox)
            (mailbox-count mailbox)))
  mailbox)

(defun mailbox-count (mailbox)
  "Returns the number of messages currently in the mailbox."
  (bt-semaphore:semaphore-count (mailbox-semaphore mailbox)))

(defun mailbox-empty-p (mailbox)
  "Returns true if MAILBOX is currently empty, NIL otherwise."
  (zerop (mailbox-count mailbox)))

(defun list-mailbox-messages (mailbox)
  "Returns a fresh list containing all the messages in the
mailbox. Does not remove messages from the mailbox."
  (bt:with-recursive-lock-held ((queues::lock-of (mailbox-queue mailbox)))
    (let* ((queue (mailbox-queue mailbox))
	   (vec (queues::elements-of queue))
	   (len (length vec)))
      (flet ((pos (x) (mod x len)))
	(loop :for i from (queues::start-of queue)
	      :below (+ (queues::start-of queue)
			(queues::size-of queue))
	      :collect	(aref vec (pos i)))))))

(defun send-message (mailbox message)
  "Adds a MESSAGE to MAILBOX. Message can be any object."
  (queues:qpush (mailbox-queue mailbox) message )
  (bt-semaphore:signal-semaphore (mailbox-semaphore mailbox)))

(defun receive-message (mailbox &key timeout)
  "Removes the oldest message from MAILBOX and returns it as the primary
value, and a secondary value of T. If MAILBOX is empty waits until a message
arrives.

If TIMEOUT is provided, and no message arrives within the specified interval,
returns primary and secondary value of NIL."
  (tagbody
     (or
      (ignore-errors
       (bt-semaphore:wait-on-semaphore (mailbox-semaphore mailbox) :timeout timeout))
      (return-from receive-message (values nil nil)))
     (multiple-value-bind (value ok) (queues:qpop (mailbox-queue mailbox))
       (if ok
	   (return-from receive-message (values value t))
	   (go :error)))
   :error
     (error "Mailbox ~S empty after WAIT-ON-SEMAPHORE."
	    mailbox)))

(defun receive-message-no-hang (mailbox)
  "The non-blocking variant of RECEIVE-MESSAGE. Returns two values,
the message removed from MAILBOX, and a flag specifying whether a
message could be received."
  (prog ((semaphore (mailbox-semaphore mailbox))
         (queue     (mailbox-queue mailbox)))
     ;; Disable interrupts, v.s.
     (unless (bt-semaphore:try-semaphore semaphore)
       (return (values nil nil)))
     (multiple-value-bind (value ok) (queues:qpop queue)
       (if ok
	   (return (values value t))
	   (go :error)))
   :error
     (error "Mailbox ~S empty after successfull TRY-SEMAPHORE."
	    mailbox)))



(defun receive-pending-messages (mailbox &optional n)
  "Removes and returns all (or at most N) currently pending messages
from MAILBOX, or returns NIL if no messages are pending.

Note: Concurrent threads may be snarfing messages during the run of
this function, so even though X,Y appear right next to each other in
the result, does not necessarily mean that Y was the message sent
right after X."
  (prog* ((msgs  '())
          (sem   (mailbox-semaphore mailbox))
          (queue (mailbox-queue mailbox))
          (avail (mailbox-count mailbox))
          (count (if n (min n avail) avail)))
     (when (zerop count)
       (go :finish))
     ;; Disable interrupts, v.s.
     (unless (bt-semaphore:try-semaphore sem count)
       (go :slow-path))
     ;; Safe because QUEUE is private; other threads may be snarfing
     ;; messages under our feet, though, hence the out of order bit
     ;; in the docstring. Same for the slow path.
     (loop
       (multiple-value-bind (msg ok) (queues:qpop queue)
	 (unless ok (go :error))
	 (push msg msgs)
	 (when (zerop (decf count))
	   (go :finish))))
     ;; This is the slow path as RECEIVE-MESSAGE-NO-HANG will have to
     ;; lock the semaphore's mutex again and again.
   :slow-path
     ;; No need for disabling interrupts because we never leave the
     ;; mailbox in an inconsistent state here.
     (loop
       (multiple-value-bind (msg ok)
           (receive-message-no-hang mailbox)
         (unless ok (go :finish))
         (push msg msgs)
         (when (zerop (decf count))
           (go :finish))))
   :finish
     (return (nreverse msgs))
   :error
     (error "Mailbox ~S empty after successfull TRY-SEMAPHORE."
	    mailbox)))
