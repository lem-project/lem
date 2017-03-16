(in-package :lem)

(export '(overlay-p
          overlay-start
          overlay-end
          overlay-attribute
          overlay-buffer
          make-overlay
          delete-overlay
          overlay-put
          overlay-get))

(defclass overlay ()
  ((start
    :initarg :start
    :reader overlay-start
    :type point)
   (end
    :initarg :end
    :reader overlay-end
    :type point)
   (attribute
    :initarg :attribute
    :reader overlay-attribute
    :type (or null attribute))
   (buffer
    :initarg :buffer
    :reader overlay-buffer
    :type buffer)
   (plist
    :initform nil
    :accessor overlay-plist
    :type list)
   (alivep
    :initform t
    :accessor overlay-alive-p
    :type boolean)))

(defun overlay-p (x)
  (typep x 'overlay))

(defun make-overlay (start end attribute)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (setf attribute (ensure-attribute attribute t))
  (let* ((buffer (point-buffer start))
         (overlay
          (make-instance 'overlay
                         :start (copy-point start :left-inserting)
                         :end (copy-point end :right-inserting)
                         :attribute attribute
                         :buffer buffer)))
    (push overlay (buffer-value buffer 'overlays))
    overlay))

(defun delete-overlay (overlay)
  (when (and (overlay-p overlay)
             (overlay-alive-p overlay))
    (delete-point (overlay-start overlay))
    (delete-point (overlay-end overlay))
    (let ((buffer (overlay-buffer overlay)))
      (setf (buffer-value buffer 'overlays)
            (delete overlay (buffer-value buffer 'overlays))))
    (setf (overlay-alive-p overlay) nil)))

(defun overlay-put (overlay key value)
  (setf (getf (overlay-plist overlay) key) value))

(defun overlay-get (overlay key)
  (getf (overlay-plist overlay) key))

(defun overlays (buffer)
  (buffer-value buffer 'overlays))
