(in-package :lem)

(defclass cursor (point)
  ((saved-column :initform nil
                 :accessor cursor-saved-column)))

(defmethod lem-base::make-buffer-point (point)
  (let ((cursor (make-instance 'cursor)))
    (copy-point-using-class cursor point :left-inserting)))

(defclass fake-cursor (point)
  ((killring :initarg :killring
             :reader fake-cursor-killring)))

(defun buffer-fake-cursors (buffer)
  (buffer-value buffer 'fake-cursors))

(defun (setf buffer-fake-cursors) (value buffer)
  (setf (buffer-value buffer 'fake-cursors) value))

(defun make-fake-cursor (point)
  (let* ((killring (copy-killring (current-killring)))
         (fake-cursor (make-instance 'fake-cursor :killring killring)))
    (copy-point-using-class fake-cursor point :left-inserting)
    (push fake-cursor (buffer-fake-cursors (point-buffer point)))
    fake-cursor))

(defun delete-fake-cursor (point)
  (let ((buffer (point-buffer point)))
    (assert (find point (buffer-fake-cursors buffer)))
    (alexandria:deletef (buffer-fake-cursors buffer) point)
    (delete-point point)
    (values)))

(defun clear-cursors (buffer)
  (mapc #'delete-fake-cursor (buffer-fake-cursors buffer))
  (values))

(defun %do-multiple-cursors (buffer function only-fake-cursors)
  (with-point ((save-point (buffer-point buffer) :left-inserting))
    (dolist (point (sort (copy-list (buffer-fake-cursors buffer)) #'point<))
      (move-point (buffer-point buffer) point)
      (with-current-killring (fake-cursor-killring point)
        (funcall function))
      (unless (point= point (buffer-point buffer))
        (move-point point (buffer-point buffer))))
    (move-point (buffer-point buffer) save-point)
    (unless only-fake-cursors
      (funcall function))))

(defmacro do-multiple-cursors ((&key (buffer '(current-buffer))
                                     (only-fake-cursors nil))
                               &body body)
  `(%do-multiple-cursors ,buffer
                         (lambda () ,@body)
                         ,only-fake-cursors))

(defun buffer-cursors (buffer)
  (sort (copy-list (cons (buffer-point buffer)
                         (buffer-fake-cursors buffer)))
        #'point<))

(defun clear-duplicate-cursors (buffer)
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :when (and next-cursor (same-line-p cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(define-command add-cursors-to-next-line () ()
  (let ((cursors (buffer-cursors (current-buffer))))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (when (and (line-offset p 1)
                           (or (null next-cursor)
                               (not (same-line-p p next-cursor))))
                  (make-fake-cursor p))))))

(define-condition garbage-collection-cursors (after-executing-command) ())
(defmethod handle-signal ((condition garbage-collection-cursors))
  (clear-duplicate-cursors (current-buffer)))

(define-condition clear-cursor-when-aborted (editor-abort-handler) ())
(defmethod handle-signal ((condition clear-cursor-when-aborted))
  (clear-cursors (current-buffer)))
