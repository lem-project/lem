(in-package :lem)

(export '(overlay-p
          overlay-start
          overlay-end
          overlay-attribute
          overlay-buffer
          make-overlay
          delete-overlay))

(defclass overlay ()
  ((start
    :initarg :start
    :reader overlay-start
    :type marker)
   (end
    :initarg :end
    :reader overlay-end
    :type marker)
   (attribute
    :initarg :attribute
    :reader overlay-attribute
    :type attribute)
   (buffer
    :initarg :buffer
    :reader overlay-buffer
    :type buffer)))

(defun overlay-p (x)
  (typep x 'overlay))

(defun make-overlay (start end attribute)
  (check-type attribute attribute)
  (assert (eq (marker-buffer start)
              (marker-buffer end)))
  (let* ((buffer (marker-buffer start))
         (overlay
          (make-instance 'overlay
                         :start (copy-marker start :right-inserting)
                         :end (copy-marker end :right-inserting)
                         :attribute attribute
                         :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (delete-point (overlay-start overlay))
    (delete-point (overlay-end overlay))
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
