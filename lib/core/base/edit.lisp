(in-package :lem-base)

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
     (with-point ((p point))
       (insert-string/point point value)
       (move-point point p)))
    ((:delete-char)
     (move-to-line point linum)
     (line-offset point 0 charpos)
     (delete-char/point point value))))

(defun apply-inverse-edit (edit point)
  (destructuring-bind (kind linum charpos value) edit
    (ecase kind
      ((:insert-char)
       (%apply-edit point :delete-char linum charpos 1))
      ((:insert-string)
       (%apply-edit point :delete-char linum charpos (length value)))
      ((:delete-char)
       (let ((charp (= 1 (length value))))
         (%apply-edit point
                      (if charp :insert-char :insert-string)
                      linum
                      charpos
                      (if charp (aref value 0) value)))))))
