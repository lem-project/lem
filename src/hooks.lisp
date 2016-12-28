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

(defmacro define-buffer-local-and-global-hook (name)
  (let ((global-name (intern (format nil "*~A*" name)))
        (keyword-name (intern (format nil "*~A*" name) :keyword)))
    `(progn

       (export ',global-name)

       (defvar ,global-name nil)

       (defun ,name (&key buffer-local-p)
         (if buffer-local-p
             (get-bvar ,keyword-name)
             (or (get-bvar ,keyword-name)
                 ,global-name)))

       (defun (setf ,name) (value &key buffer-local-p)
         (if buffer-local-p
             (setf (get-bvar ,keyword-name) value)
             (if (get-bvar ,keyword-name)
                 (setf (get-bvar ,keyword-name) value)
                 (setf ,global-name value)))))))
