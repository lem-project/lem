(in-package :lem)

(export '(current-point
          pointp
          copy-point
          delete-point
          point-buffer
          point-linum
          point-charpos
          point-kind

          point=
          point/=
          point<
          point<=
          point>
          point>=))

(defclass point ()
  ((buffer
    :initarg :buffer
    :accessor point-buffer
    :type buffer)
   (line
    :initarg :line
    :accessor point-line
    :type line)
   (linum
    :initarg :linum
    :accessor point-linum
    :type fixnum)
   (charpos
    :initarg :charpos
    :accessor point-charpos
    :type fixnum)
   (kind
    :initarg :kind
    :accessor point-kind
    :type (member :temporary :left-inserting :right-inserting))
   (name
    :initarg :name
    :accessor point-name
    :type (or null string))))

(defun current-point ()
  (buffer-point (current-buffer)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "POINT ~A (~A ~A)"
            (point-name object)
            (point-linum object)
            (point-charpos object))))

(defun pointp (x)
  (typep x 'point))

(defun make-point (buffer linum charpos &key (kind :right-inserting) name)
  (let ((point (make-instance 'point
                              :buffer buffer
                              :line (buffer-get-line buffer linum)
                              :linum linum
                              :charpos charpos
                              :kind kind
                              :name name)))
    (unless (eq :temporary kind)
      (buffer-add-point buffer point))
    point))

(defun copy-point (point &optional kind)
  (make-point (point-buffer point)
	      (point-linum point)
	      (point-charpos point)
	      :kind (or kind (point-kind point))
	      :name (point-name point)))

(defun delete-point (point)
  (unless (eq :temporary (point-kind point))
    (buffer-delete-point (point-buffer point)
			 point)))

(defun point-change-buffer (point buffer)
  (delete-point point)
  (unless (eq :temporary (point-kind point))
    (buffer-add-point buffer point))
  (setf (point-buffer point) buffer)
  t)

(defun point= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (and (= (point-linum point1)
          (point-linum point2))
       (= (point-charpos point1)
          (point-charpos point2))))

(defun point/= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (not (point= point1 point2)))

(defun point< (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (cond ((< (point-linum point1) (point-linum point2))
         t)
        ((> (point-linum point1) (point-linum point2))
         nil)
        ((< (point-charpos point1) (point-charpos point2))
         t)
        (t
         nil)))

(defun point<= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (or (point< point1 point2)
      (point= point1 point2)))

(defun point> (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point< point2 point1))

(defun point>= (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (point<= point2 point1))
