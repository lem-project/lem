(in-package :lem/directory-mode)

(defun run-command (command)
  (when (consp command)
    (setf command (mapcar #'princ-to-string command)))
  (let ((error-string
          (with-output-to-string (error-output)
            (uiop:run-program command
                              :ignore-error-status t
                              :error-output error-output))))
    (when (string/= error-string "")
      (editor-error "~A" error-string))))
