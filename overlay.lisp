(in-package :lem)

(defstruct (overlay (:constructor make-overlay-internal))
  start
  end
  prop
  buffer)

(defun make-overlay (start end &key prop (buffer (current-buffer)))
  (let ((overlay
         (make-overlay-internal :start start
                                :end end
                                :prop prop
                                :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (buffer-delete-overlay (overlay-buffer overlay) overlay))
