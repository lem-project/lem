(in-package :lem)

(defclass cursor (point)
  ((saved-column :initform nil
                 :accessor cursor-saved-column)
   (yank-start :initform nil
               :accessor cursor-yank-start)
   (yank-end :initform nil
             :accessor cursor-yank-end)))

(defmethod lem-base::make-buffer-point (point)
  (let ((cursor (make-instance 'cursor)))
    (copy-point-using-class cursor point :left-inserting)))

(defclass fake-cursor (cursor)
  ((killring :initarg :killring
             :reader fake-cursor-killring)
   (mark :initarg :mark
         :initform (make-instance 'mark)
         :reader fake-cursor-mark)))

(defmethod cursor-mark ((cursor cursor))
  (buffer-mark-object (point-buffer cursor)))

(defmethod cursor-mark ((cursor fake-cursor))
  (fake-cursor-mark cursor))

(defmethod set-cursor-mark ((cursor cursor))
  (set-current-mark cursor))

(defmethod set-cursor-mark ((cursor fake-cursor))
  (mark-set-point (fake-cursor-mark cursor) cursor))

(defmethod cursor-region-beginning ((cursor cursor))
  (point-min cursor (mark-point (cursor-mark cursor))))

(defmethod cursor-region-end ((cursor cursor))
  (point-max cursor (mark-point (cursor-mark cursor))))

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
    ;; (assert (find point (buffer-fake-cursors buffer)))
    (alexandria:deletef (buffer-fake-cursors buffer) point)
    (delete-point point)
    (values)))

(defun clear-cursors (buffer)
  (mapc #'delete-fake-cursor (buffer-fake-cursors buffer))
  (values))

(defun %do-multiple-cursors (buffer function only-fake-cursors)
  (dolist (point (sort (copy-list (buffer-fake-cursors buffer)) #'point<))
    (lem-base::with-buffer-point (buffer point)
      (with-current-killring (fake-cursor-killring point)
        (funcall function))))
  (unless only-fake-cursors
    (funcall function)))

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
