(in-package :lem-core)

(defclass <overlay> ()
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

(defclass temporary-overlay (<overlay>) ())

(defclass overlay (<overlay>) ())

(defclass overlay-line-endings (overlay)
  ((offset :initarg :offset
           :initform 0
           :reader overlay-line-endings-offset)
   (text :initarg :text
         :initform (alexandria:required-argument :text)
         :accessor overlay-line-endings-text)))

(defmethod initialize-instance ((overlay <overlay>) &key &allow-other-keys)
  (let ((overlay (call-next-method)))
    (with-slots (start end attribute) overlay
      (when (point< end start) (rotatef start end))
      (setf attribute (ensure-attribute attribute t)))
    overlay))

(defmethod initialize-instance :after ((overlay overlay) &key &allow-other-keys)
  (push overlay (buffer-value (overlay-buffer overlay) 'overlays)))

(defun make-overlay (start end attribute
                     &key (start-point-kind :right-inserting)
                          (end-point-kind :left-inserting))
  (make-instance 'overlay
                 :start (copy-point start start-point-kind)
                 :end (copy-point end end-point-kind)
                 :attribute attribute
                 :buffer (point-buffer start)))

(defun make-overlay-line-endings (start end attribute
                                  &key (start-point-kind :right-inserting)
                                       (end-point-kind :left-inserting)
                                       (text (alexandria:required-argument :text))
                                       (offset 0))
  (make-instance 'overlay-line-endings
                 :start (copy-point start start-point-kind)
                 :end (copy-point end end-point-kind)
                 :attribute attribute
                 :buffer (point-buffer start)
                 :text text
                 :offset offset))

(defun delete-overlay (overlay)
  (check-type overlay overlay)
  (when (and (overlay-alive-p overlay)
             (not (typep overlay 'temporary-overlay)))
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

(defun buffer-overlays (buffer)
  (buffer-value buffer 'overlays))

(defun clear-overlays (buffer)
  (mapc #'delete-overlay (buffer-overlays buffer)))

(defun point-overlays (point)
  (loop :for ov :in (buffer-overlays (point-buffer point))
        :when (point<= (overlay-start ov) point (overlay-end ov))
        :collect ov))
