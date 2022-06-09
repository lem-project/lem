(in-package :lem)

;; (pushnew :lem.features.multiple-cursors *features*)

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

(defun buffer-cursors (buffer)
  (cons (buffer-point buffer)
        (buffer-fake-cursors buffer)))

#+lem.features.multiple-cursors
(define-command add-cursors-to-next-line () ()
  (let ((cursors (sort (copy-list (buffer-cursors (current-buffer))) #'point<)))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p 1)
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (add-fake-cursor p))))))

#+lem.features.multiple-cursors
(define-command clear-cursors () ()
  (delete-all-fake-cursors (current-buffer)))

(defun foreach-multiple-cursors (buffer function)
  (dolist (point (buffer-fake-cursors buffer))
    (funcall function point))
  (funcall function (buffer-point buffer)))

(defmacro do-multiple-cursors ((point buffer) &body body)
  `(foreach-multiple-cursors ,buffer (lambda (,point) ,@body)))
