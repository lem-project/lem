(in-package :lem)

(export '(timer
          timer-p
          timer-ms
          start-timer
          stop-timer
          alive-timer-p))

(defvar *timer-list* nil)
(defvar *idle-timer-list* nil)

(defclass timer ()
  ((ms
    :initarg :ms
    :reader timer-ms
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
    :type function)
   (args
    :initarg :args
    :reader timer-args
    :type list)
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))))

(defun timer-p (x)
  (typep x 'timer))

(defun start-timer (ms repeat-p function &optional args handle-function)
  (let ((timer (make-instance 'timer
                              :ms ms
                              :repeat-p repeat-p
                              :last-time (get-internal-real-time)
                              :function function
                              :args args
                              :handle-function handle-function)))
    (push timer *timer-list*)
    timer))

(defun stop-timer (timer)
  (setq *timer-list* (delete timer *timer-list*)))

(defun update-timer ()
  (let ((promised-timers)
        (update-timers))
    (dolist (timer *timer-list*)
      (when (< (timer-ms timer)
               (- (get-internal-real-time)
                  (timer-last-time timer)))
        (push timer update-timers)
        (if (timer-repeat-p timer)
            (setf (timer-last-time timer)
                  (get-internal-real-time))
            (push timer promised-timers))))
    (setq *timer-list* (set-difference *timer-list* promised-timers))
    (dolist (timer update-timers)
      (handler-case
          (if (timer-handle-function timer)
              (handler-bind ((error (timer-handle-function timer)))
                (apply (timer-function timer) (timer-args timer)))
              (apply (timer-function timer) (timer-args timer)))
        (error (condition)
               (message "Error running timer: ~a" condition))))
    (not (null update-timers))))

(defun shortest-wait-timers ()
  (let ((list (mapcar (lambda (timer)
                        (- (timer-ms timer)
                           (- (get-internal-real-time)
                              (timer-last-time timer))))
                      *timer-list*)))
    (if (null list)
        nil
        (reduce #'min list))))

(defun exist-running-timer-p ()
  (not (null *timer-list*)))
