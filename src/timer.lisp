(in-package :lem)

(defvar *is-in-idle* nil)
(defvar *timer-list* nil)
(defvar *idle-timer-list* nil)
(defvar *processed-idle-timer-list* nil)
(defvar *running-timer* nil)

(defun running-timer () *running-timer*)

(defun get-microsecond-time ()
  (values
   (floor (/ (get-internal-real-time)
             (load-time-value (/ internal-time-units-per-second 1000))))))

(defclass timer ()
  ((name
    :initarg :name
    :reader timer-name
    :type (or null string))
   (ms
    :initarg :ms
    :accessor timer-ms
    :type (integer 1 *))
   (repeat-p
    :initarg :repeat-p
    :reader timer-repeat-p
    :type boolean)
   (last-time
    :initarg :last-time
    :accessor timer-last-time
    :type (integer 1 *))
   (function
    :initarg :function
    :reader timer-function
    :type (or symbol function))
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))
   (expired-p
    :initform nil
    :reader timer-expired-p
    :writer set-timer-expired-p
    :type boolean)
   (idle-p
    :initarg :idle-p
    :accessor timer-idle-p
    :type boolean)))

(defun timer-has-last-time (timer)
  (slot-boundp timer 'last-time))

(defmethod print-object ((object timer) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (prin1 (timer-name object) stream)))

(defun timer-next-time (timer)
  (+ (timer-last-time timer) (timer-ms timer)))

(defun expire-timer (timer)
  (set-timer-expired-p t timer))

(defun inspire-timer (timer)
  (set-timer-expired-p nil timer))

(defun start-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'timer
                              :name (or name
                                        (and (symbolp function)
                                             (symbol-name function)))
                              :ms ms
                              :repeat-p repeat-p
                              :last-time (get-microsecond-time)
                              :function function
                              :handle-function handle-function
                              :idle-p nil)))
    (push timer *timer-list*)
    timer))

(defun stop-timer (timer)
  (expire-timer timer)
  (if (timer-idle-p timer)
      (setf *idle-timer-list* (delete timer *idle-timer-list*))
      (setf *timer-list* (delete timer *timer-list*))))

(defun start-idle-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'timer
                              :name (or name (and (symbolp function)
                                                  (symbol-name function)))
                              :ms ms
                              :repeat-p repeat-p
                              :function function
                              :handle-function handle-function
                              :idle-p t)))
    (push timer *idle-timer-list*)
    timer))

(defun start-idle-timers ()
  (flet ((update-last-time-in-idle-timers ()
           (loop :with last-time := (get-microsecond-time)
                 :for timer :in *idle-timer-list*
                 :do (setf (timer-last-time timer) last-time)))
         (inspire-idle-timers ()
           (mapc #'inspire-timer *idle-timer-list*)))
    (update-last-time-in-idle-timers)
    (inspire-idle-timers)))

(defun stop-idle-timers ()
  (setf *idle-timer-list* (nconc *processed-idle-timer-list* *idle-timer-list*))
  (setf *processed-idle-timer-list* '()))

(defun call-with-idle-timers (function)
  (start-idle-timers)
  (prog1 (let ((*is-in-idle* t))
           (funcall function))
    (stop-idle-timers)))

(defmacro with-idle-timers (() &body body)
  `(call-with-idle-timers (lambda () ,@body)))

(defun run-timer (timer)
  (handler-case
      (let ((*running-timer* timer))
        (if (timer-handle-function timer)
            (handler-bind ((error (timer-handle-function timer)))
              (funcall (timer-function timer)))
            (funcall (timer-function timer))))
    (error (condition)
      (show-message (format nil "Error running timer ~S: ~A" (timer-name timer) condition)))))

(defun update-timer ()
  (let* ((tick-time (get-microsecond-time))
         (target-timers (if *is-in-idle*
                            (append *timer-list* *idle-timer-list*)
                            *timer-list*))
         (updating-timers (remove-if-not (lambda (timer)
                                           (< (timer-next-time timer) tick-time))
                                         (remove-if-not #'timer-has-last-time target-timers)))
         (deleting-timers (remove-if-not (lambda (timer)
                                           (not (timer-repeat-p timer)))
                                         updating-timers))
         (updating-idle-timers (if *is-in-idle*
                                   (remove-if-not (lambda (timer)
                                                    (and (timer-idle-p timer)
                                                         (timer-repeat-p timer)))
                                                  updating-timers)
                                   '())))
    (dolist (timer deleting-timers)
      (expire-timer timer))
    ;; Not so efficient, but it will be enough.
    (setf *timer-list* (set-difference *timer-list* deleting-timers))
    (when *is-in-idle*
      (setf *idle-timer-list* (set-difference *idle-timer-list* deleting-timers))
      (setf *idle-timer-list* (set-difference *idle-timer-list* updating-idle-timers))
      (setf *processed-idle-timer-list* (nconc updating-idle-timers *processed-idle-timer-list*)))

    (dolist (timer updating-timers)
      (unless (and (timer-idle-p timer)
                   (timer-repeat-p timer))
        (setf (timer-last-time timer) tick-time)))
    (mapc #'run-timer updating-timers)
    (redraw-display)
    (not (null updating-timers))))

(defun get-next-timer-timing-ms ()
  (let ((timers (if *is-in-idle*
                    (append *timer-list* *idle-timer-list*)
                    *timer-list*)))
    ;;Remove timers without a last-time
    (setf timers (remove-if-not #'timer-has-last-time timers))
    (if (null timers)
        nil
        (- (loop :for timer :in timers
                 :minimize (timer-next-time timer))
           (get-microsecond-time)))))
