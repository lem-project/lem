(in-package :lem)

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
  (setf (window-cur-col) (point-column point)))
