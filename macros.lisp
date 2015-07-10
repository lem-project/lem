(in-package :lem)

(export '(save-excursion))

(defmacro when-interrupted-flag (flag &body body)
  (let ((gflag (gensym "FLAG")))
    `(let ((,gflag ,flag))
       (unless (cdr (assoc ,flag *last-flags*)) ,@body)
       (push (cons ,gflag t) *last-flags*)
       (push (cons ,gflag t) *curr-flags*))))

(defmacro when-continue-flag (flag &body body)
  (let ((gflag (gensym "FLAG")))
    `(let ((,gflag ,flag))
       (when (cdr (assoc ,flag *last-flags*)) ,@body)
       (push (cons ,gflag t) *last-flags*)
       (push (cons ,gflag t) *curr-flags*))))

(defmacro save-excursion (&body body)
  (let ((gpoint (gensym "POINT")))
    `(let ((,gpoint (point)))
       (unwind-protect (progn ,@body)
         (point-set ,gpoint)))))
