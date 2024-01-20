(in-package :lem-base/buffer)

(defstruct per-buffer-hook
  var
  buffer
  (global t))

(defmethod run-hooks ((hook per-buffer-hook) &rest args)
  (with-slots (var buffer global) hook
    (apply #'run-hooks (variable-value var :buffer buffer) args)
    (when global
      (apply #'run-hooks (variable-value var :global) args))))

(defun clear-editor-local-variables (buffer)
  "`buffer`の全てのバッファローカルなエディタ変数を未束縛にします。"
  (dolist (symbol (editor-variables))
    (buffer-unbound buffer
                    (editor-variable-local-indicator
                     (get symbol 'editor-variable)))))

(defmethod variable-value-aux ((var editor-variable) (kind (eql :default)) &optional (where nil wherep))
  (let* ((buffer (if wherep
                     (ensure-buffer where)
                     (current-buffer)))
         (default '#:default)
         (value (buffer-value buffer
                              (editor-variable-local-indicator var)
                              default)))
    (if (eq value default)
        (editor-variable-value var)
        value)))

(defmethod variable-value-aux ((var editor-variable) (kind (eql :buffer)) &optional (where nil wherep))
  (let ((buffer (if wherep
                    (ensure-buffer where)
                    (current-buffer))))
    (buffer-value buffer
                  (editor-variable-local-indicator var))))

(defun set-variable-value-aux-default (var value where wherep)
  (let ((buffer (if wherep
                    (ensure-buffer where)
                    (current-buffer))))
    (setf (buffer-value buffer
                        (editor-variable-local-indicator var))
          value)))

(defmethod (setf variable-value-aux)
    (value (var editor-variable) (kind (eql :buffer)) &optional (where nil wherep))
  (set-variable-value-aux-default var value where wherep))

(defmethod (setf variable-value-aux)
    (value (var editor-variable) (kind (eql :default)) &optional (where nil wherep))
  (set-variable-value-aux-default var value where wherep))
