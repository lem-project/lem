(in-package :lem-core)

(export '(run-hooks add-hook))

(defun run-hooks (hook &rest args)
  (dolist (fn (get hook 'hooks))
    (apply fn args)))

(defun add-hook (hook callback)
  (pushnew callback (get hook 'hooks)))
