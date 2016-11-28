(in-package :lem)

(export '(point-p
          point
          make-point
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

(defun point-p (point)
  (and (consp point)
       (integerp (car point))
       (<= 1 (car point))
       (integerp (cdr point))
       (<= 0 (car point))))

(deftype point ()
  `(satisfies point-p))

(defun make-point (linum charpos)
  (cons linum charpos))

(defun make-min-point ()
  (make-point 1 0))

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

(defun current-linum ()
  (window-current-linum))

(defun (setf current-linum) (new-linum)
  (assert (<= 1 new-linum (buffer-nlines (current-buffer))))
  (setf (window-current-linum) new-linum))

(defun current-charpos ()
  (window-current-charpos))

(defun (setf current-charpos) (new-charpos)
  (assert (<= 0 new-charpos (buffer-line-length (current-buffer) (current-linum))))
  (setf (window-current-charpos) new-charpos))

(defun current-point ()
  (make-point
   (current-linum)
   (current-charpos)))

(defun (setf current-point) (new-point)
  (point-set new-point)
  new-point)

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
