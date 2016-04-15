;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(make-point
          point-linum
          point-charpos
          with-points
          current-point
          current-linum
          current-charpos
          point-set
          point<
          point=
          point<=
          point>
          point>=
          point-min
          point-max))

(defun make-point (linum charpos)
  (list linum charpos))

(defun point-linum (point)
  (car point))

(defun point-charpos (point)
  (cadr point))

(defmacro with-points (binds &body body)
  `(let ,(mapcan #'(lambda (b)
                     `((,(caar b) (point-linum ,(cadr b)))
                       (,(cadar b) (point-charpos ,(cadr b)))))
                 binds)
     ,@body))

(defun current-linum ()
  (window-current-linum))

(defun (setf current-linum) (new-linum)
  (setf (window-current-linum) new-linum))

(defun current-charpos ()
  (window-current-charpos))

(defun (setf current-charpos) (new-charpos)
  (setf (window-current-charpos) new-charpos))

(defun current-point ()
  (make-point
   (current-linum)
   (current-charpos)))

(defun (setf current-point) (new-point)
  (point-set new-point))

(defun point-set (point &optional (window (current-window)))
  (setf (window-current-linum window)
        (min (buffer-nlines (window-buffer window))
             (point-linum point)))
  (setf (window-current-charpos window)
        (min (buffer-line-length (window-buffer window)
                                 (window-current-linum window))
             (point-charpos point)))
  (assert (<= 0 (window-current-charpos window))))

(defun point< (p1 p2)
  (cond ((< (point-linum p1) (point-linum p2))
         t)
        ((> (point-linum p1) (point-linum p2))
         nil)
        ((< (point-charpos p1) (point-charpos p2))
         t)
        (t
         nil)))

(defun point= (p1 p2)
  (equal p1 p2))

(defun point<= (p1 p2)
  (or (point< p1 p2)
      (point= p1 p2)))

(defun point> (p1 p2)
  (point< p2 p1))

(defun point>= (p1 p2)
  (point<= p2 p1))

(defun point-min (&optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (make-point 1 0))

(defun point-max (&optional (buffer (current-buffer)))
  (buffer-end-point buffer))

(defun adjust-point (&optional (point (current-point)))
  (point-set point))
