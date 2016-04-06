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

(defun make-overlay (start end &key attr (buffer (window-buffer)))
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


(export '(put-attribute
          remove-attribute
          current-column
          move-to-column))

(defun put-attribute (start end attr)
  (buffer-put-attribute (window-buffer) start end attr))

(defun remove-attribute (start end attr)
  (buffer-remove-attribute (window-buffer) start end attr))

(defun current-column ()
  (str-width (buffer-line-string (window-buffer)
                                 (window-current-linum))
             0
             (window-current-charpos)))

(defun move-to-column (column &optional force)
  (check-type column (integer 0 #.most-positive-fixnum))
  (end-of-line)
  (let ((current-column (current-column)))
    (cond ((< column current-column)
           (set-charpos (wide-index (buffer-line-string
                                     (window-buffer)
                                     (window-current-linum))
                                    column))
           column)
          (force
           (insert-char #\space (- column current-column))
           (end-of-line)
           column)
          (t
           (end-of-line)
           current-column))))
