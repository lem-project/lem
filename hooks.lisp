(in-package :lem)

(defun run-hooks (hook)
  (pdebug (cons hook (get hook 'hooks)))
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))
