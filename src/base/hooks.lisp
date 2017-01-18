(in-package :lem-base)

(export '(run-hooks add-hook))

(defun run-hooks (hooks &rest args)
  (dolist (hook hooks)
    (apply (car hook) args)))

(defmacro add-hook (place callback &optional (weight 0))
  `(setf ,place
         (merge 'list
                (list (cons ,callback ,weight))
                ,place
                #'>
                :key #'cdr)))
