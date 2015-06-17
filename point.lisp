(in-package :lem)

(defun make-point (linum column)
  (list linum column))

(defmacro with-points (binds &body body)
  `(let ,(mapcan (lambda (b)
                   `((,(caar b) (car ,(cadr b)))
                     (,(cadar b) (cadr ,(cadr b)))))
           binds)
     ,@body))

(defun point ()
  (make-point
   (window-cur-linum)
   (window-cur-col)))
