(in-package :lem)

(export '(point
          make-min-point
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

(deftype point ()
  `(satisfies point-p))

(defun make-point (linum charpos)
  (cons linum charpos))

(defun make-min-point ()
  (make-point 1 0))

(defun point-linum (point)
  (car point))

(defun (setf point-linum) (linum point)
  (setf (car point) linum))

(defun point-charpos (point)
  (cdr point))

(defun (setf point-charpos) (charpos point)
  (setf (cdr point) charpos))

(defmacro with-points (binds &body body)
  `(let ,(mapcan #'(lambda (b)
                     `((,(caar b) (point-linum ,(cadr b)))
                       (,(cadar b) (point-charpos ,(cadr b)))))
                 binds)
     ,@body))

(defun current-linum ()
  (marker-linum (buffer-point-marker (current-buffer))))

(defun (setf current-linum) (new-linum)
  (check-linum (current-buffer) new-linum)
  (setf (marker-linum (buffer-point-marker (current-buffer))) new-linum))

(defun current-charpos ()
  (marker-charpos (buffer-point-marker (current-buffer))))

(defun (setf current-charpos) (new-charpos)
  (check-point (current-buffer) (current-linum) new-charpos)
  (setf (marker-charpos (buffer-point-marker (current-buffer))) new-charpos))

(defun current-point ()
  (marker-point (buffer-point-marker (current-buffer))))

(defun (setf current-point) (new-point)
  (point-set new-point)
  new-point)

(defun round-linum (buffer linum)
  (cond ((minusp linum) 1)
        ((<= linum (buffer-nlines buffer))
         linum)
        (t
         (buffer-nlines buffer))))

(defun point-set (point &optional (buffer (current-buffer)))
  (setf (marker-point (buffer-point-marker buffer))
        (let ((linum (min (buffer-nlines buffer)
                          (point-linum point))))
          (make-point linum
                      (max 0
                           (min (buffer-line-length buffer linum)
                                (point-charpos point)))))))

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
  (make-min-point))

(defun point-max (&optional (buffer (current-buffer)))
  (buffer-end-point buffer))
