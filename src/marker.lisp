(in-package :lem)

(export '(current-marker
          markerp
          make-marker
          copy-point
          delete-point
          marker-buffer
          marker-linum
          marker-charpos
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
    :accessor marker-linum
    :type fixnum)
   (charpos
    :initarg :charpos
    :accessor marker-charpos
    :type fixnum)
   (kind
    :initarg :kind
    :accessor marker-kind
    :type (member :temporary :left-inserting :right-inserting))
   (name
    :initarg :name
    :accessor marker-name
    :type (or null string))))

(defun current-marker ()
  (buffer-point-marker (current-buffer)))

(defmethod print-object ((object marker) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "MARKER ~A (~A ~A)"
            (marker-name object)
            (marker-linum object)
            (marker-charpos object))))

(defun marker-p (x)
  (typep x 'marker))

(defun make-marker (buffer linum charpos &key (kind :right-inserting) name)
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
  (make-marker (marker-buffer marker)
               (marker-linum marker)
               (marker-charpos marker)
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
  (and (= (marker-linum marker1)
          (marker-linum marker2))
       (= (marker-charpos marker1)
          (marker-charpos marker2))))

(defun point/= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (not (point= marker1 marker2)))

(defun point< (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (cond ((< (marker-linum marker1) (marker-linum marker2))
         t)
        ((> (marker-linum marker1) (marker-linum marker2))
         nil)
        ((< (marker-charpos marker1) (marker-charpos marker2))
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
