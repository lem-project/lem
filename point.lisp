;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(make-point
          point-linum
          point-charpos
          with-points
          current-point
          point-set
          point<
          point=
          point<=
          point>
          point>=
          point-shift
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

(defun current-point ()
  (make-point
   (window-current-linum)
   (window-current-charpos)))

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

(defun point-shift (point n)
  (save-excursion
   (point-set point)
   (next-char n)
   (current-point)))

(defun point-min (&optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (make-point 1 0))

(defun point-max (&optional (buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer nil)
    (end-of-buffer)
    (current-point)))

(defun adjust-point (&optional (point (current-point)))
  (point-set point))
