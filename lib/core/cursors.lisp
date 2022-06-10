(in-package :lem)

(defun buffer-fake-cursors (buffer)
  (buffer-value buffer 'fake-cursors))

(defun (setf buffer-fake-cursors) (value buffer)
  (setf (buffer-value buffer 'fake-cursors) value))

(defun add-fake-cursor (point)
  (push (copy-point point :left-inserting)
        (buffer-fake-cursors (point-buffer point))))

(defun delete-fake-cursor (point)
  (let ((buffer (point-buffer point)))
    (assert (find point (buffer-fake-cursors buffer)))
    (alexandria:deletef (buffer-fake-cursors buffer) point)
    (delete-point point)
    (values)))

(defun delete-all-fake-cursors (buffer)
  (mapc #'delete-fake-cursor (buffer-fake-cursors buffer))
  (values))

(defun foreach-multiple-cursors (buffer function)
  (dolist (point (buffer-fake-cursors buffer))
    (funcall function point))
  (funcall function (buffer-point buffer)))

(defmacro do-multiple-cursors ((point buffer) &body body)
  `(foreach-multiple-cursors ,buffer (lambda (,point) ,@body)))

(defun call-with-multiple-cursors (buffer function)
  (with-point ((save-point (buffer-point buffer) :left-inserting))
    (dolist (point (sort (copy-list (buffer-fake-cursors buffer)) #'point<))
      (move-point (buffer-point buffer) point)
      (funcall function)
      (unless (point= point (buffer-point buffer))
        (move-point point (buffer-point buffer))))
    (move-point (buffer-point buffer) save-point))
  (funcall function))

(defmacro with-multiple-cursors ((&optional (buffer '(current-buffer))) &body body)
  `(call-with-multiple-cursors ,buffer (lambda () ,@body)))

(defun buffer-cursors (buffer)
  (sort (copy-list (cons (buffer-point buffer)
                         (buffer-fake-cursors buffer)))
        #'point<))

(define-command add-cursors-to-next-line () ()
  (let ((cursors (buffer-cursors (current-buffer))))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p 1)
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (add-fake-cursor p))))))

(define-command clear-cursors () ()
  (delete-all-fake-cursors (current-buffer)))
