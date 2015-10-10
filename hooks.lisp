;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(run-hooks
          add-hook))

(defun run-hooks (hook)
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))
