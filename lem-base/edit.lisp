(in-package :lem-base)

(declaim (inline edit))
(defun make-edit (kind linum charpos value)
  (list kind linum charpos value))

(defun %apply-edit (point kind linum charpos value)
  (ecase kind
    ((:insert-char)
     (move-to-line point linum)
     (line-offset point 0 charpos)
     (insert-char/point point value))
    ((:insert-string)
     (move-to-line point linum)
     (line-offset point 0 charpos)
     (insert-string/point point value))
    ((:delete-char)
     (move-to-line point linum)
     (line-offset point 0 charpos)
     (delete-char/point point value))))

(defun apply-edit (edit point)
  (destructuring-bind (kind linum charpos value) edit
    (%apply-edit point kind linum charpos value)))

(defun apply-inverse-edit (edit point)
  (destructuring-bind (kind linum charpos value) edit
    (ecase kind
      ((:insert-char)
       (%apply-edit point :delete-char linum charpos 1))
      ((:insert-string)
       (%apply-edit point :delete-char linum charpos (length value)))
      ((:delete-char)
       (%apply-edit point :insert-string linum charpos value)))))
