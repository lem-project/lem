(in-package :lem)

(export '(overlay-p
          make-overlay
          delete-overlay))

(defclass overlay ()
  ((start
    :initarg :start
    :reader overlay-start)
   (end
    :initarg :end
    :reader overlay-end)
   (attr
    :initarg :attr
    :reader overlay-attr)
   (buffer
    :initarg :buffer
    :reader overlay-buffer)))

(defun overlay-p (x)
  (typep x 'overlay))

(defun make-overlay (start end attribute &optional (buffer (current-buffer)))
  (check-type attribute attribute)
  (let ((overlay
          (make-instance 'overlay
                         :start start
                         :end end
                         :attr attribute
                         :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
