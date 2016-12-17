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

(defun make-overlay (start-marker end-marker attribute)
  (check-type attribute attribute)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (let* ((buffer (marker-buffer start-marker))
         (overlay
          (make-instance 'overlay
                         :start start-marker
                         :end end-marker
                         :attribute attribute
                         :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (delete-marker (overlay-start overlay))
    (delete-marker (overlay-end overlay))
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
