;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(run-hooks
          add-hook
          timer
          timer-p
          timer-ms
          timer-repeat-p
          timer-last-time
          timer-function
          start-timer
          stop-timer
          alive-timer-p
          overlay
          make-overlay
          delete-overlay))

(defun run-hooks (hook)
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))

(defvar *timer-list* nil)

(defstruct (timer (:constructor %make-timer))
  ms
  repeat-p
  last-time
  function)

(defun start-timer (ms repeat-p function)
  (let ((timer
         (%make-timer :ms ms
                      :repeat-p repeat-p
                      :last-time (get-internal-real-time)
                      :function function)))
    (push timer *timer-list*)
    timer))

(defun stop-timer (timer)
  (setq *timer-list* (delete timer *timer-list*)))

(defun update-timer ()
  (let ((promised-timers)
        (update-p))
    (dolist (timer *timer-list*)
      (when (< (timer-ms timer)
               (- (get-internal-real-time)
                  (timer-last-time timer)))
        (funcall (timer-function timer))
        (setq update-p t)
        (if (timer-repeat-p timer)
            (setf (timer-last-time timer)
                  (get-internal-real-time))
            (push timer promised-timers))))
    (setq *timer-list* (set-difference *timer-list* promised-timers))
    update-p))

(defstruct (overlay (:constructor make-overlay-internal))
  start
  end
  attr
  buffer)

(defun make-overlay (start end &key attr (buffer (window-buffer)))
  (let ((overlay
         (make-overlay-internal :start start
                                :end end
                                :attr attr
                                :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
