(in-package :lem)

(defmacro when-interrupted-flag (flag-name &body body)
  (let ((name (intern (string-upcase (format nil "flags-~a" flag-name)))))
    `(progn
       (unless (,name *last-flags*)
         ,@body)
       (setf (,name *last-flags*) t)
       (setf (,name *curr-flags*) t))))

(defmacro save-excursion (&body body)
  (let ((gpoint (gensym "POINT")))
    `(let ((,gpoint (point)))
       (unwind-protect (progn ,@body)
         (point-set ,gpoint)))))
