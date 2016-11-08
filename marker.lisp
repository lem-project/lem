(in-package :lem)

(export '(markerp
          make-marker
          make-marker-current-point
          delete-marker
          marker-buffer
          marker-linum
          marker-charpos
          marker-point
          marker-insertion-type))

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
   (insertion-type
    :initarg :insertion-type
    :accessor marker-insertion-type
    :type boolean)
   (name
    :initarg :name
    :accessor marker-name
    :type (or null string))))

(defmethod print-object ((object marker) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "MARKER ~A"
            (marker-name object))))

(defun marker-p (x)
  (typep x 'marker))

(defun make-marker (buffer point &key insertion-type name)
  (let ((marker (make-instance 'marker
                               :buffer buffer
                               :linum (point-linum point)
                               :charpos (point-charpos point)
                               :insertion-type insertion-type
                               :name name)))
    (buffer-add-marker buffer marker)
    marker))

(defun make-marker-current-point (&key insertion-type name)
  (make-marker (current-buffer)
               (current-point)
               :insertion-type insertion-type
               :name name))

(defun delete-marker (marker)
  (buffer-delete-marker (marker-buffer marker)
                        marker))

(defun marker-point (marker)
  (make-point (marker-linum marker)
              (marker-charpos marker)))

(defun (setf marker-point) (new-point marker)
  (setf (marker-linum marker) (point-linum new-point)
        (marker-charpos marker) (point-charpos new-point))
  new-point)

(defun marker-change-buffer (marker buffer &optional (point nil pointp))
  (delete-marker marker)
  (buffer-add-marker buffer marker)
  (setf (marker-buffer marker) buffer)
  (when pointp
    (setf (marker-point marker) point))
  t)
