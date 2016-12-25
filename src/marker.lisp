(in-package :lem)

(export '(current-point
          pointp
          make-point
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

(defun point-change-buffer (marker buffer)
  (delete-point marker)
  (unless (eq :temporary (point-kind marker))
    (buffer-add-point buffer marker))
  (setf (point-buffer marker) buffer)
  t)

(defun point= (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (and (= (point-linum marker1)
          (point-linum marker2))
       (= (point-charpos marker1)
          (point-charpos marker2))))

(defun point/= (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (not (point= marker1 marker2)))

(defun point< (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (cond ((< (point-linum marker1) (point-linum marker2))
         t)
        ((> (point-linum marker1) (point-linum marker2))
         nil)
        ((< (point-charpos marker1) (point-charpos marker2))
         t)
        (t
         nil)))

(defun point<= (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (or (point< marker1 marker2)
      (point= marker1 marker2)))

(defun point> (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (point< marker2 marker1))

(defun point>= (marker1 marker2)
  (assert (eq (point-buffer marker1)
              (point-buffer marker2)))
  (point<= marker2 marker1))
