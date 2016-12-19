(in-package :lem)

(export '(current-marker
          markerp
          make-marker
          make-marker-current-point
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
    (format stream "MARKER ~A ~A"
            (marker-name object)
            (marker-point object))))

(defun marker-p (x)
  (typep x 'marker))

(defun make-marker (buffer point &key (kind :right-inserting) name)
  (let ((marker (make-instance 'marker
                               :buffer buffer
                               :linum (point-linum point)
                               :charpos (point-charpos point)
                               :kind kind
                               :name name)))
    (unless (eq :temporary kind)
      (buffer-add-marker buffer marker))
    marker))

(defun make-marker-current-point (&key (kind :right-inserting) name)
  (make-marker (current-buffer)
               (current-point)
               :kind kind
               :name name))

(defun copy-marker (marker &optional kind)
  (make-marker (marker-buffer marker)
               (copy-list (marker-point marker))
               :kind (or kind (marker-kind marker))
               :name (marker-name marker)))

(defun delete-marker (marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-delete-marker (marker-buffer marker)
                          marker)))

(defun marker-point (marker)
  (make-point (marker-linum marker)
              (marker-charpos marker)))

(defun (setf marker-point) (new-point marker)
  (setf (marker-linum marker) (point-linum new-point)
        (marker-charpos marker) (point-charpos new-point))
  new-point)

(defun marker-change-buffer (marker buffer &optional (point nil pointp))
  (delete-marker marker)
  (unless (eq :temporary (marker-kind marker))
    (buffer-add-marker buffer marker))
  (setf (marker-buffer marker) buffer)
  (when pointp
    (setf (marker-point marker) point))
  t)


;;; 同じバッファ同士かの比較は保留

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
