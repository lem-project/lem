;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(overlay
          make-overlay
          delete-overlay))

(defstruct (overlay (:constructor %make-overlay))
  start
  end
  attr
  buffer)

(defun make-overlay (start end &key attr (buffer (current-buffer)))
  (let ((overlay
         (%make-overlay :start start
                        :end end
                        :attr attr
                        :buffer buffer)))
    (buffer-add-overlay buffer overlay)
    overlay))

(defun delete-overlay (overlay)
  (when (overlay-p overlay)
    (buffer-delete-overlay (overlay-buffer overlay) overlay)))
