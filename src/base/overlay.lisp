(in-package :lem-base)

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
    :type point)
   (end
    :initarg :end
    :reader overlay-end
    :type point)
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
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (let* ((buffer (point-buffer start))
         (overlay
          (make-instance 'overlay
                         :start (copy-point start :right-inserting)
                         :end (copy-point end :right-inserting)
                         :attribute attribute
                         :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (delete-point (overlay-start overlay))
    (delete-point (overlay-end overlay))
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
