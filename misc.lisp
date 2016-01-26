;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(run-hooks
          add-hook
          find-file-hook
          before-save-hook
          after-save-hook
          idle-hook))

(defun run-hooks (hook)
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))


(export '(timer
          timer-p
          timer-ms
          start-timer
          stop-timer
          alive-timer-p))

(defvar *timer-list* nil)

(defstruct (timer (:constructor %make-timer))
  _ms
  repeat-p
  last-time
  function
  args
  handler-function)

(defun start-timer (ms repeat-p function &optional args handler-function)
  (let ((timer
         (%make-timer :_ms ms
                      :repeat-p repeat-p
                      :last-time (get-internal-real-time)
                      :function function
                      :args args
                      :handler-function handler-function)))
    (push timer *timer-list*)
    timer))

(defun stop-timer (timer)
  (setq *timer-list* (delete timer *timer-list*)))

(defun timer-ms (timer)
  (timer-_ms timer))

(defun (setf timer-ms) (new-ms timer)
  (check-type new-ms (integer 1 #.most-positive-fixnum))
  (setf (timer-_ms timer) new-ms))

(defun update-timer ()
  (let ((promised-timers)
        (update-p))
    (dolist (timer *timer-list*)
      (when (< (timer-_ms timer)
               (- (get-internal-real-time)
                  (timer-last-time timer)))
        (handler-case
            (if (timer-handler-function timer)
                (handler-bind ((error (timer-handler-function timer)))
                  (apply (timer-function timer) (timer-args timer)))
                (apply (timer-function timer) (timer-args timer)))
          (error (condition)
                 (minibuf-print
                  (format nil "Error running timer: ~a" condition))))
        (setq update-p t)
        (if (timer-repeat-p timer)
            (setf (timer-last-time timer)
                  (get-internal-real-time))
            (push timer promised-timers))))
    (setq *timer-list* (set-difference *timer-list* promised-timers))
    update-p))

(defun exist-running-timer-p ()
  (not (null *timer-list*)))


(export '(overlay
          make-overlay
          delete-overlay))

(defstruct (overlay (:constructor %make-overlay))
  start
  end
  attr
  buffer)

(defun make-overlay (start end &key attr (buffer (window-buffer)))
  (let ((overlay
         (%make-overlay :start start
                        :end end
                        :attr attr
                        :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))


(export '(make-marker
          marker-p
          delete-marker
          marker-buffer
          marker-linum
          marker-column
          marker-point))

(deftype marker ()
  `(satisfies markerp))

(let ((marker-tag (gensym "MARKER")))
  (defun make-marker (&optional (point (point)) (buffer (window-buffer)))
    (let ((marker (vector marker-tag
                          buffer
                          (point-linum point)
                          (point-column point))))
      (buffer-add-marker buffer marker)
      marker))
  (defun marker-p (x)
    (and (vectorp x)
         (eq marker-tag (safe-aref x 0)))))

(defun delete-marker (marker)
  (buffer-delete-marker
   (marker-buffer marker)
   marker))

(defun marker-buffer (marker)
  (aref marker 1))

(defun marker-linum (marker)
  (aref marker 2))

(defun (setf marker-linum) (new-linum marker)
  (setf (aref marker 2) new-linum))

(defun marker-column (marker)
  (aref marker 3))

(defun (setf marker-column) (new-column marker)
  (setf (aref marker 3) new-column))

(defun marker-point (marker)
  (make-point (marker-linum marker)
              (marker-column marker)))

(defun (setf marker-point) (new-point marker)
  (setf (marker-linum marker) (point-linum new-point)
        (marker-column marker) (point-column new-point))
  new-point)


(export '(put-attribute
          remove-attribute
          current-column
          move-to-column))

(defun put-attribute (start end attr)
  (buffer-put-attribute (window-buffer) start end attr))

(defun remove-attribute (start end attr)
  (buffer-remove-attribute (window-buffer) start end attr))

(defun current-column ()
  (str-width (buffer-line-string (window-buffer)
                                 (window-cur-linum))
             0
             (window-cur-col)))

(defun move-to-column (column &optional force)
  (end-of-line)
  (let ((current-column (current-column)))
    (cond ((< column current-column)
           (goto-column (wide-index (buffer-line-string
                                     (window-buffer)
                                     (window-cur-linum))
                                    (1+ column)))
           column)
          (force
           (insert-char #\space (- column current-column))
           (end-of-line)
           column)
          (t
           (end-of-line)
           current-column))))
