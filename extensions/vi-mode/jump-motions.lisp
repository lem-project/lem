(defpackage #:lem-vi-mode/jump-motions
  (:use #:cl
        #:lem)
  (:export #:with-jump-motion
           #:jump-back
           #:jump-next))
(in-package #:lem-vi-mode/jump-motions)

(defvar *prev-jump-points* '())
(defvar *current-point* nil)
(defvar *next-jump-points* '())

(defvar *jump-motion-recursive* nil)
(defmacro with-jump-motion (&body body)
  (let ((p (gensym "P")))
    `(if *jump-motion-recursive*
         (progn ,@body)
         (let ((*jump-motion-recursive* t)
               (,p (copy-point (current-point)) ; leak?
                   ))
           (prog1 (progn ,@body)
             (unless (point= ,p (current-point))
               (push ,p *prev-jump-points*))
             (setf *current-point* nil))))))

(defun jump-back ()
  (push (or *current-point*
            (copy-point (current-point))) ; leak?
        *next-jump-points*)
  (setf *current-point* (pop *prev-jump-points*))
  (when *current-point*
    (switch-to-buffer (point-buffer *current-point*))
    (move-point (current-point) *current-point*)))

(defun jump-next ()
  (when *current-point*
    (push *current-point* *prev-jump-points*))
  (let ((p (pop *next-jump-points*)))
    (setf *current-point* p)
    (when *current-point*
      (switch-to-buffer (point-buffer *current-point*))
      (move-point (current-point) p))))
