(in-package :lem)

(export '(point-linum
          point-charpos
          with-points
          point<
          point=
          point<=
          point>
          point>=))

(defun point-linum (point)
  (car point))

(defun point-charpos (point)
  (cdr point))

(defmacro with-points (binds &body body)
  `(let ,(mapcan #'(lambda (b)
                     `((,(caar b) (point-linum ,(cadr b)))
                       (,(cadar b) (point-charpos ,(cadr b)))))
                 binds)
     ,@body))

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
