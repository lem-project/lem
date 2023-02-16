(in-package :lem)

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
    :writer set-overlay-attribute
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

(defun make-temporary-overlay (start end attribute)
  (when (point< end start) (rotatef start end))
  (let* ((attribute (ensure-attribute attribute t))
         (buffer (point-buffer start))
         (overlay
           (make-instance 'overlay
                          :start start
                          :end end
                          :attribute attribute
                          :buffer buffer)))
    overlay))

(defun make-overlay (start end attribute)
  (when (point< end start) (rotatef start end))
  (let* ((attribute (ensure-attribute attribute t))
         (buffer (point-buffer start))
         (overlay
           (make-instance 'overlay
                          :start (copy-point start :right-inserting)
                          :end (copy-point end :left-inserting)
                          :attribute attribute
                          :buffer buffer)))
    (push overlay (buffer-value buffer 'overlays))
    overlay))

(defun delete-overlay (overlay)
  (check-type overlay overlay)
  (when (overlay-alive-p overlay)
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

(defun clear-overlays (buffer)
  (mapc #'delete-overlay (overlays buffer)))

(defun point-overlays (point)
  (loop :for ov :in (overlays (point-buffer point))
        :when (point<= (overlay-start ov) point (overlay-end ov))
        :collect ov))
