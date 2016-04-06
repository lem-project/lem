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
    :reader marker-buffer
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
    :type boolean)))

(defun marker-p (x)
  (typep x 'marker))

(defun make-marker (buffer point &optional insertion-type)
  (let ((marker (make-instance 'marker
                               :buffer buffer
                               :linum (point-linum point)
                               :charpos (point-charpos point)
                               :insertion-type insertion-type)))
    (buffer-add-marker buffer marker)
    marker))

(defun make-marker-current-point (&optional insertion-type)
  (make-marker (current-buffer) (current-point) insertion-type))

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
