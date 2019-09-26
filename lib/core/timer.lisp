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

(defun timer-next-time (timer)
  (+ (timer-last-time timer) (timer-ms timer)))

(defun start-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'timer
                              :name (or name
                                        (and (symbolp function)
                                             (symbol-name function)))
                              :ms ms
                              :repeat-p repeat-p
                              :last-time (get-internal-real-time)
                              :function function
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
  (let* ((tick-time (get-internal-real-time))
         (updating-timers (remove-if-not (lambda (timer)
                                           (< (timer-next-time timer) tick-time))
                                         *timer-list*))
         (deleting-timers (remove-if-not (lambda (timer)
                                           (or (timer-idle-p timer)
                                               (not (timer-repeat-p timer))))
                                         updating-timers)))
    (dolist (timer deleting-timers)
      (unless (and (timer-idle-p timer)
                   (timer-repeat-p timer))
        (setf (timer-alive-p timer) nil)))
    ;; Not so efficient, but it will be enough.
    (setf *timer-list* (set-difference *timer-list* deleting-timers))

    (dolist (timer updating-timers)
      (unless (and (timer-idle-p timer)
                   (timer-repeat-p timer))
        (setf (timer-last-time timer) tick-time)))
    (dolist (timer updating-timers)
      (handler-case
          (if (timer-handle-function timer)
              (handler-bind ((error (timer-handle-function timer)))
                (funcall (timer-function timer)))
              (funcall (timer-function timer)))
        (error (condition)
          (message "Error running timer ~S: ~A" (timer-name timer) condition))))
    (redraw-display)
    (not (null updating-timers))))

(defun shortest-wait-timers ()
  (if (null *timer-list*)
      nil
      (- (loop :for timer :in *timer-list*
               :minimize (timer-next-time timer))
         (get-internal-real-time))))

(defun exist-running-timer-p ()
  (not (null *timer-list*)))

(defun start-idle-timer (ms repeat-p function &optional handle-function name)
  (let ((timer (make-instance 'timer
                              :name (or name (and (symbolp function)
                                                  (symbol-name function)))
                              :ms ms
                              :repeat-p repeat-p
                              :function function
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
