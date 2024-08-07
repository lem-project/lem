(in-package :lem/buffer/internal)

(deftype edit-kind ()
  '(member :insert-string :delete-string))

(defstruct (edit (:constructor make-edit (kind position string)))
  (kind (alexandria:required-argument :kind)
        :type edit-kind
        :read-only t)
  (position (alexandria:required-argument :position)
            :type (integer 0 *)
            :read-only t)
  (string (alexandria:required-argument :string)
          :type string
          :read-only t))

(defun apply-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (move-to-position point (edit-position edit))
     (with-point ((p point))
       (insert-string/point point (edit-string edit))
       (move-point point p)))
    ((:delete-string)
     (move-to-position point (edit-position edit))
     (delete-char/point point (length (edit-string edit))))))

(defun apply-inverse-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (apply-edit point
                 (make-edit :delete-string
                            (edit-position edit)
                            (edit-string edit))))
    ((:delete-string)
     (apply-edit point
                 (make-edit :insert-string
                            (edit-position edit)
                            (edit-string edit))))))
