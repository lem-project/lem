(in-package :lem)

(export '(timer
          timer-p
          timer-name
          timer-ms
          timer-alive-p
          start-timer
          start-idle-timer
          stop-timer
          alive-timer-p))

(defvar *timer-list* nil)
(defvar *idle-timer-list* nil)

(defclass timer ()
  ((name
    :initarg :name
    :reader timer-name
    :type simple-string)
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
    :type function)
   (args
    :initarg :args
    :reader timer-args
    :type list)
   (handle-function
    :initarg :handle-function
    :reader timer-handle-function
    :type (or null function))
   (alive-p
    :initarg :alive-p
    :accessor timer-alive-p
    :type boolean)
   (idle-p
    :initarg :idle-p
    :accessor timer-idle-p
    :type boolean)))

(defmethod print-object ((object timer) stream)
  (print-unreadable-object (object stream :identity t)
                           (format stream "TIMER: ~S" (timer-name object))))

(defun timer-p (x)
  (typep x 'timer))

(defun start-timer (name ms repeat-p function &optional args handle-function)
  (let ((timer (make-instance 'timer
                              :name name
                              :ms ms
                              :repeat-p repeat-p
                              :last-time (get-internal-real-time)
                              :function function
                              :args args
                              :handle-function handle-function
                              :alive-p t
                              :idle-p nil)))
    (push timer *timer-list*)
    timer))

(defun stop-timer (timer)
  (cond
    ((timer-idle-p timer)
     (setf (timer-alive-p timer) nil)
     (setf *idle-timer-list* (delete timer *idle-timer-list*))
     (setf *timer-list* (delete timer *timer-list*)))
    (t
     (setf (timer-alive-p timer) nil)
     (setf *timer-list* (delete timer *timer-list*)))))

(defun update-timer ()
  (let ((promised-timers)
        (update-timers))
    (dolist (timer *timer-list*)
      (when (< (timer-ms timer)
               (- (get-internal-real-time)
                  (timer-last-time timer)))
        (push timer update-timers)
        (cond ((and (timer-repeat-p timer)
                    (not (timer-idle-p timer)))
               (setf (timer-last-time timer)
                     (get-internal-real-time)))
              (t
               (unless (timer-idle-p timer)
                 (setf (timer-alive-p timer) nil))
               (push timer promised-timers)))))
    (setq *timer-list* (set-difference *timer-list* promised-timers))
    (dolist (timer update-timers)
      (handler-case
          (if (timer-handle-function timer)
              (handler-bind ((error (timer-handle-function timer)))
                (apply (timer-function timer) (timer-args timer)))
              (apply (timer-function timer) (timer-args timer)))
        (error (condition)
               (message "Error running timer ~S: ~A" (timer-name timer) condition)
               (redraw-display))))
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

(defun start-idle-timer (name ms repeat-p function &optional args handle-function)
  (let ((timer (make-instance 'timer
                              :name name
                              :ms ms
                              :repeat-p repeat-p
                              :function function
                              :args args
                              :handle-function handle-function
                              :alive-p t
                              :idle-p t)))
    (push timer *idle-timer-list*)
    timer))

(defun start-idle-timers ()
  (dolist (timer *idle-timer-list*)
    (setf (timer-last-time timer) (get-internal-real-time))
    (setf (timer-alive-p timer) t)
    (push timer *timer-list*)))

(defun stop-idle-timers ()
  (let ((new-idle-timers))
    (dolist (timer *idle-timer-list*)
      (when (timer-repeat-p timer)
        (push timer new-idle-timers))
      (setf *timer-list* (delete timer *timer-list*)))
    (setf *idle-timer-list* new-idle-timers)))
