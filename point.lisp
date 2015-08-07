(in-package :lem)

(export '(make-point
          point-linum
          point-column
          with-points
          point
          point-set
          point<
          point<=
          point-max))

(defun make-point (linum column)
  (list linum column))

(defun point-linum (point)
  (car point))

(defun point-column (point)
  (cadr point))

(defmacro with-points (binds &body body)
  `(let ,(mapcan (lambda (b)
                   `((,(caar b) (point-linum ,(cadr b)))
                     (,(cadar b) (point-column ,(cadr b)))))
           binds)
     ,@body))

(defun point ()
  (make-point
   (window-cur-linum)
   (window-cur-col)))

(defun point-set (point)
  (setf (window-cur-linum) (point-linum point))
  (setf (window-cur-col) (point-column point))
  (setf (window-max-col) (point-column point)))

(defun point< (p1 p2)
  (cond ((< (point-linum p1) (point-linum p2))
         t)
        ((> (point-linum p1) (point-linum p2))
         nil)
        ((< (point-column p1) (point-column p2))
         t)
        (t
         nil)))

(defun point= (p1 p2)
  (equal p1 p2))

(defun point<= (p1 p2)
  (or (point< p1 p2)
      (point= p1 p2)))

(defun point-max ()
  (save-excursion
   (end-of-buffer)
   (point)))
