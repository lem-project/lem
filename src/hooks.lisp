(in-package :lem)

(export '(run-hooks add-hook))

(defun run-hooks (hook &rest args)
  (dolist (fn (get hook 'hooks))
    (apply fn args)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))
