(in-package :lem-base)

(export '(run-hooks add-hook))

(defun run-hooks (functions &rest args)
  (dolist (fn functions)
    (apply fn args)))

(defmacro add-hook (place callback)
  `(pushnew ,callback ,place))
