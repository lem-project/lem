(in-package :lem)

(export '(current-marker
          markerp
          make-marker
          copy-marker
          delete-marker
          marker-buffer
          marker-linum
          marker-charpos
          marker-point
          marker-kind

          marker=
          marker/=
          marker<
          marker<=
          marker>
          marker>=))

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
    :type (or (eql :temporary) (eql :left-inserting) (eql :right-inserting)))
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

(defun copy-marker (marker &optional kind)
  (make-marker (marker-buffer marker)
               (marker-linum marker)
               (marker-charpos marker)
               :kind (or kind (marker-kind marker))
               :name (marker-name marker)))

(defun delete-marker (marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-delete-marker (marker-buffer marker)
                          marker)))

(defun marker-point (marker)
  (cons (marker-linum marker)
        (marker-charpos marker)))

(defun (setf marker-point) (new-point marker)
  (let ((buffer (marker-buffer marker)))
    (setf (marker-linum marker)
          (min (point-linum new-point)
               (buffer-nlines buffer)))
    (setf (marker-charpos marker)
          (min (buffer-line-length buffer (marker-linum marker))
               (point-charpos new-point))))
  new-point)

(defun marker-change-buffer (marker buffer &optional (point nil pointp))
  (delete-marker marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-add-marker buffer marker))
  (setf (marker-buffer marker) buffer)
  (when pointp
    (move-point marker point))
  t)

(defun marker= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point= (marker-point marker1)
          (marker-point marker2)))

(defun marker/= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (not (point= (marker-point marker1)
               (marker-point marker2))))

(defun marker< (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point< (marker-point marker1)
          (marker-point marker2)))

(defun marker<= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point<= (marker-point marker1)
           (marker-point marker2)))

(defun marker> (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point> (marker-point marker1)
          (marker-point marker2)))

(defun marker>= (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (point>= (marker-point marker1)
           (marker-point marker2)))
