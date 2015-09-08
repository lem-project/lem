(in-package :lem)

(defstruct (overlay (:constructor make-overlay-internal))
  start
  end
  attr
  buffer)

(defun make-overlay (start end &key attr (buffer (current-buffer)))
  (let ((overlay
         (make-overlay-internal :start start
                                :end end
                                :attr attr
                                :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (buffer-delete-overlay (overlay-buffer overlay) overlay))
