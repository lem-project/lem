;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(put-attribute
          remove-attribute
          current-column
          move-to-column))

(defun put-attribute (start end attr)
  (buffer-put-attribute (current-buffer) start end attr))

(defun remove-attribute (start end attr)
  (buffer-remove-attribute (current-buffer) start end attr))

(defun current-column ()
  (str-width (buffer-line-string (current-buffer)
                                 (current-linum))
             0
             (current-charpos)))

(defun move-to-column (column &optional force)
  (check-type column (integer 0 #.most-positive-fixnum))
  (end-of-line)
  (let ((current-column (current-column)))
    (cond ((< column current-column)
           (set-charpos (wide-index (buffer-line-string
                                     (current-buffer)
                                     (current-linum))
                                    column))
           column)
          (force
           (insert-char #\space (- column current-column))
           (end-of-line)
           column)
          (t
           (end-of-line)
           current-column))))
