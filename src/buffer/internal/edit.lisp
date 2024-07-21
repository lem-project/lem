(in-package :lem/buffer/internal)

(deftype edit-kind ()
  '(member :insert-string :delete-string))

(defstruct (edit (:constructor make-edit (kind linum charpos string)))
  (kind (alexandria:required-argument :kind)
        :type edit-kind
        :read-only t)
  (linum (alexandria:required-argument :linum)
         :type (integer 1 *)
         :read-only t)
  (charpos (alexandria:required-argument :linum)
           :type (integer 0 *)
           :read-only t)
  (string (alexandria:required-argument :string)
          :type string
          :read-only t))

(defun apply-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (move-to-line point (edit-linum edit))
     (line-offset point 0 (edit-charpos edit))
     (with-point ((p point))
       (insert-string/point point (edit-string edit))
       (move-point point p)))
    ((:delete-string)
     (move-to-line point (edit-linum edit))
     (line-offset point 0 (edit-charpos edit))
     (delete-char/point point (length (edit-string edit))))))

(defun apply-inverse-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (apply-edit point
                 (make-edit :delete-string
                            (edit-linum edit)
                            (edit-charpos edit)
                            (edit-string edit))))
    ((:delete-string)
     (apply-edit point
                 (make-edit :insert-string
                            (edit-linum edit)
                            (edit-charpos edit)
                            (edit-string edit))))))
