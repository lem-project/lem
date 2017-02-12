(in-package :lem-base)

(export '(run-hooks add-hook remove-hook))

(defun run-hooks (hooks &rest args)
  (dolist (hook hooks)
    (apply (car hook) args)))

(defmacro add-hook (place callback &optional (weight 0))
  (let ((_callback (gensym)))
    `(let ((,_callback ,callback))
       (unless (member ,_callback ,place :key #'car)
         (setf ,place
               (merge 'list
                      (list (cons ,_callback ,weight))
                      ,place
                      #'>
                      :key #'cdr))))))

(defmacro remove-hook (place callback)
  `(setf ,place (delete ,callback ,place :key #'car)))
