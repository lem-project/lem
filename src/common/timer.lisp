(defpackage :lem/common/timer
  (:use :cl)
  (:export :timer-error
           :running-timer
           :timer
           :timer-name
           :timer-expired-p
           :start-timer
           :start-idle-timer
           :stop-timer
           :with-idle-timers
           :update-timers
           :get-next-timer-timing-ms))
(in-package :lem/common/timer)

(defvar *is-in-idle* nil)
(defvar *timer-list* nil)
(defvar *idle-timer-list* nil)
(defvar *processed-idle-timer-list* nil)
(defvar *running-timer* nil)

(define-condition timer-error (error)
  ((timer :initarg :timer)
   (condition :initarg :condition))
  (:report (lambda (c s)
             (with-slots (timer condition) c
               (format s "Error running timer ~S: ~A" (timer-name timer) condition)))))

(defun running-timer () *running-timer*)

(defun get-microsecond-time ()
  (values
   (floor (/ (get-internal-real-time)
             (load-time-value (/ internal-time-units-per-second 1000))))))

(defclass timer-internal ()
  ((ms
    :initarg :ms
    :accessor timer-internal-ms
    :type (integer 1 *))
   (repeat-p
    :initarg :repeat-p
    :reader timer-internal-repeat-p
    :type boolean)
   (expired-p
    :initform nil
    :reader timer-internal-expired-p
    :writer set-timer-internal-expired-p
    :type boolean)
   (last-time
    :initform nil
    :initarg :last-time
    :accessor timer-internal-last-time
    :type (or null (integer 1 *)))))

(defclass <timer> ()
  ((name
    :initarg :name
    :reader timer-name
    :type (or null string))
   (function
    :initarg :function
    :reader timer-function
    :type (or symbol function))
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))
   (state
    :initarg :state
    :reader timer-internal
    :type timer-internal)))

(defclass timer (<timer>)
  ())

(defclass idle-timer (<timer>)
  ())

(defmethod print-object ((object timer) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (prin1 (timer-name object) stream)))

(defgeneric idle-timer-p (timer)
  (:method ((timer timer)) nil)
  (:method ((timer idle-timer)) t))

(defun timer-ms (timer)
  (timer-internal-ms (timer-internal timer)))

(defun timer-repeat-p (timer)
  (timer-internal-repeat-p (timer-internal timer)))

(defun timer-expired-p (timer)
  (timer-internal-expired-p (timer-internal timer)))

(defun timer-last-time (timer)
  (timer-internal-last-time (timer-internal timer)))

(defun set-timer-last-time (value timer)
  (setf (timer-internal-last-time (timer-internal timer)) value))

(defun timer-has-last-time (timer)
  (not (null (timer-last-time timer))))

(defun timer-next-time (timer)
  (+ (timer-last-time timer) (timer-ms timer)))

(defun expire-timer (timer)
  (set-timer-internal-expired-p t (timer-internal timer)))

(defun inspire-timer (timer)
  (set-timer-internal-expired-p nil (timer-internal timer)))

(defun start-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'timer
                              :name (or name
                                        (and (symbolp function)
                                             (symbol-name function)))
                              :function (alexandria:ensure-function function)
                              :handle-function (when handle-function
                                                 (alexandria:ensure-function handle-function))
                              :state (make-instance 'timer-internal
                                                    :ms ms
                                                    :repeat-p repeat-p
                                                    :last-time (get-microsecond-time)))))
    (push timer *timer-list*)
    timer))

(defun start-idle-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'idle-timer
                              :name (or name (and (symbolp function)
                                                  (symbol-name function)))
                              :function (alexandria:ensure-function function)
                              :handle-function (when handle-function
                                                 (alexandria:ensure-function handle-function))
                              :state (make-instance 'timer-internal
                                                    :ms ms
                                                    :repeat-p repeat-p))))
    (push timer *idle-timer-list*)
    timer))

(defun stop-timer (timer)
  (expire-timer timer)
  (if (idle-timer-p timer)
      (setf *idle-timer-list* (delete timer *idle-timer-list*))
      (setf *timer-list* (delete timer *timer-list*))))

(defun start-idle-timers ()
  (flet ((update-last-time-in-idle-timers ()
           (loop :with last-time := (get-microsecond-time)
                 :for timer :in *idle-timer-list*
                 :do (set-timer-last-time last-time timer)))
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

(defun call-timer-function (timer)
  (handler-case
      (let ((*running-timer* timer))
        (if (timer-handle-function timer)
            (handler-bind ((error (timer-handle-function timer)))
              (funcall (timer-function timer)))
            (funcall (timer-function timer))))
    (error (condition)
      (error 'timer-error :timer timer :condition condition))))

(defun update-timers ()
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
                                                    (and (idle-timer-p timer)
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
      (unless (and (idle-timer-p timer)
                   (timer-repeat-p timer))
        (set-timer-last-time tick-time timer)))
    (mapc #'call-timer-function updating-timers)
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
