(in-package :lem)

(export '(run-hooks
          add-hook
          find-file-hook
          before-save-hook
          after-save-hook
          pre-command-hook
          post-command-hook
          after-init-hook
          exit-editor-hook))

(defun run-hooks (hook)
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))
