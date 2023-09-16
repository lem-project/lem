(defpackage :lem-vi-mode/jumplist
  (:use :cl
        :lem)
  (:export :with-jump-motion
           :jump-back
           :jump-next
           :window-jumplist))
(in-package :lem-vi-mode/jumplist)

(defvar *max-jumplist-size* 100)

(defstruct jumplist
  (history nil :type list)
  (index 0 :type (integer 0))
  (current nil :type (or null point)))

(defmethod print-object ((object jumplist) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (history index current)
        object
      (format stream ":HISTORY (~A items) :INDEX ~A"
              (+ (length history)
                 (if current 1 0))
              index))))

(define-condition jumplist-invalid-index (error)
  ((jumplist :initarg :jumplist)
   (index :initarg :index))
  (:report (lambda (condition stream)
             (with-slots (jumplist index) condition
               (format stream "Invalid index ~A for ~A"
                       index
                       jumplist)))))

(defun jumplist-ref (jumplist index)
  (check-type index integer)
  (cond
    ((= index 0)
     (jumplist-current jumplist))
    ((< 0 index)
     (or (nth (1- index)
              (jumplist-history jumplist))
         (error 'jumplist-invalid-index
                :jumplist jumplist
                :index index)))
    (t
     (error 'jumplist-invalid-index
            :jumplist jumplist
            :index index))))

(defun delete-exceeded-elements (list count)
  (let ((last-cdr (nthcdr (- count 1) list)))
    (when last-cdr
      (setf (cdr last-cdr) nil))
    list))

(defun jumplist-push (jumplist point)
  (with-slots (history index current) jumplist
    ;; Delete the newer history
    (when (< 0 index)
      (setf history (nthcdr index history))
      (setf index 0
            current nil))
    (setf history
          (cons point
                ;; Remove points at the same line in the history
                (remove (line-number-at-point point)
                        history
                        :key #'line-number-at-point
                        :test #'=
                        :count 1)))
    ;; Keep only *max-jumplist-size* points (including `current`)
    (delete-exceeded-elements history (1- *max-jumplist-size*))
    point))

(defun jumplist-history-back (jumplist)
  (with-slots (index current) jumplist
    (when (zerop index)
      (setf current (copy-point (current-point))))
    (handler-case
        (prog1 (jumplist-ref jumplist (1+ index))
          (incf index))
      (jumplist-invalid-index () nil))))

(defun jumplist-history-next (jumplist)
  (with-slots (index current) jumplist
    (when (<= 0 (1- index))
      (decf index)
      (prog1 (jumplist-ref jumplist index)
        (when (zerop index)
          (setf current nil))))))

(defun window-jumplist (&optional (window (current-window)))
  (or (window-parameter window :vi-mode-jumplist)
      (setf (window-parameter window :vi-mode-jumplist)
            (make-jumplist))))

(defun jumplist-table (&optional (jumplist (window-jumplist)))
  (flet ((buffer-identifier (buffer)
           (if (buffer-filename buffer)
               (enough-namestring (buffer-filename buffer))
               (buffer-name buffer))))
    (with-slots (index current) jumplist
      (loop with results = (list (list (= 0 index)
                                       (abs (- 0 index))
                                       (and current
                                            (line-number-at-point current))
                                       (and current
                                            (point-column current))
                                       (and current
                                            (buffer-identifier (point-buffer current)))))
            for point in (jumplist-history jumplist)
            for i from 1
            ;; current-p index line column file/buffer-name
            do (push (list
                      (= i index)
                      (abs (- i index))
                      (line-number-at-point point)
                      (point-column point)
                      (buffer-identifier (point-buffer point)))
                     results)
            finally (return results)))))

(defun print-jumplist (&optional (jumplist (window-jumplist)))
  (let ((table (jumplist-table jumplist)))
    (message
     (with-output-to-string (s)
       (dolist (row table)
         (destructuring-bind (current-p index line column file)
             row
           (format s "~:[  ~;> ~]~A: ~A~%"
                   current-p
                   index
                   (and line column file
                        (format nil "(~A, ~A)  ~A" line column file)))))))))

(defvar *jump-motion-recursive* nil)
(defun call-with-jump-motion (fn)
  (let ((*jump-motion-recursive* t)
        (p (copy-point (current-point))))
    (prog1 (funcall fn)
      (unless (point= p (current-point))
        (jumplist-push (window-jumplist) p)))))

(defmacro with-jump-motion (&body body)
  `(if *jump-motion-recursive*
       (progn ,@body)
       (call-with-jump-motion
        (lambda () ,@body))))

(defun jump-back ()
  (let ((point (jumplist-history-back (window-jumplist))))
    (when point
      (switch-to-buffer (point-buffer point))
      (move-point (current-point) point)
      point)))

(defun jump-next ()
  (let ((point (jumplist-history-next (window-jumplist))))
    (when point
      (switch-to-buffer (point-buffer point))
      (move-point (current-point) point))))
