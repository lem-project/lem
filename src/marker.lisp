(in-package :lem)

(export '(current-point
          markerp
          make-point
          copy-point
          delete-point
          marker-buffer
          point-linum
          point-charpos
          marker-kind

          point=
          point/=
          point<
          point<=
          point>
          point>=))

(defclass marker ()
  ((buffer
    :initarg :buffer
    :accessor marker-buffer
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
    :accessor marker-kind
    :type (member :temporary :left-inserting :right-inserting))
   (name
    :initarg :name
    :accessor marker-name
    :type (or null string))))

(defun current-point ()
  (buffer-point-marker (current-buffer)))

(defmethod print-object ((object marker) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "MARKER ~A (~A ~A)"
            (marker-name object)
            (point-linum object)
            (point-charpos object))))

(defun pointp (x)
  (typep x 'marker))

(defun make-point (buffer linum charpos &key (kind :right-inserting) name)
  (let ((marker (make-instance 'marker
                               :buffer buffer
                               :linum linum
                               :charpos charpos
                               :kind kind
                               :name name)))
    (unless (eq :temporary kind)
      (buffer-add-marker buffer marker))
    marker))

(defun copy-point (marker &optional kind)
  (make-point (marker-buffer marker)
               (point-linum marker)
               (point-charpos marker)
               :kind (or kind (marker-kind marker))
               :name (marker-name marker)))

(defun delete-point (marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-delete-marker (marker-buffer marker)
                          marker)))

(defun point-change-buffer (marker buffer &optional (point nil pointp))
  (delete-point marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-add-marker buffer marker))
  (setf (marker-buffer marker) buffer)
  (when pointp
    (move-point marker point))
  t)

(defun point= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (and (= (point-linum marker1)
          (point-linum marker2))
       (= (point-charpos marker1)
          (point-charpos marker2))))

(defun point/= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (not (point= marker1 marker2)))

(defun point< (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (cond ((< (point-linum marker1) (point-linum marker2))
         t)
        ((> (point-linum marker1) (point-linum marker2))
         nil)
        ((< (point-charpos marker1) (point-charpos marker2))
         t)
        (t
         nil)))

(defun point<= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (or (point< marker1 marker2)
      (point= marker1 marker2)))

(defun point> (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point< marker2 marker1))

(defun point>= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point<= marker2 marker1))
