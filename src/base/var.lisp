(in-package :lem-base)

(defvar *editor-variables* '())

(defstruct editor-variable
  value
  documentation
  local-indicator
  change-value-hook)

(setf (documentation 'editor-variable 'type)
      "`editor-variable`はエディタ内で使われる変数です。
バッファローカルな変数や大域的な値を管理するために使います。")

(defmacro define-editor-variable (var &optional value documentation change-value-hook)
  "エディタ変数`var`を定義します。
`value`はそのエディタ変数に束縛されている大域的な値です。
`documentation`はそのエディタ変数の文書文字列です。
`change-value-hook`はそのエディタ変数の大域的な値が変更されるときに呼び出されるフック関数です。
"
  (check-type var symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (get ',var 'editor-variable)
       (defvar ,var)
       (pushnew ',var *editor-variables*)
       (setf (get ',var 'editor-variable)
             (make-editor-variable :value ,value
                                   :documentation ,documentation
                                   :local-indicator (gensym ,(string var))
                                   :change-value-hook ,change-value-hook))
       t)))

(defun clear-editor-local-variables (buffer)
  "`buffer`の全てのバッファローカルなエディタ変数を未束縛にします。"
  (dolist (symbol *editor-variables*)
    (buffer-unbound buffer
                    (editor-variable-local-indicator
                     (get symbol 'editor-variable)))))

(defun editor-variable-error (symbol)
  (error "~A is not editor variable" symbol))

(defun check-editor-variable (symbol)
  (unless (editor-variable-p (get symbol 'editor-variable))
    (editor-variable-error symbol)))

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

(defmethod variable-value-aux ((var editor-variable) (kind (eql :global)) &optional (where nil wherep))
  (declare (ignore where wherep))
  (editor-variable-value var))

(defun variable-value (symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (if wherep
        (variable-value-aux var kind where)
        (variable-value-aux var kind))))

(defun set-variable-value-aux-default (var value where wherep)
  (let ((buffer (if wherep
                    (ensure-buffer where)
                    (current-buffer))))
    (setf (buffer-value buffer
                        (editor-variable-local-indicator var))
          value)))

(defmethod (setf variable-value-aux) (value (var editor-variable) (kind (eql :default)) &optional (where nil wherep))
  (set-variable-value-aux-default var value where wherep))

(defmethod (setf variable-value-aux) (value (var editor-variable) (kind (eql :buffer)) &optional (where nil wherep))
  (set-variable-value-aux-default var value where wherep))

(defmethod (setf variable-value-aux) (value (var editor-variable) (kind (eql :global)) &optional (where nil wherep))
  (declare (ignore where wherep))
  (let ((fn (editor-variable-change-value-hook var)))
    (when fn
      (funcall fn value)))
  (setf (editor-variable-value var) value))

(defun (setf variable-value) (value symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (if wherep
        (setf (variable-value-aux var kind where) value)
        (setf (variable-value-aux var kind) value))))

(defun variable-documentation (symbol)
  "エディタ変数`symbol`の文書文字列を返します。"
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (editor-variable-documentation var)))

(defun find-editor-variable (var)
  (find var *editor-variables* :test 'string-equal))

(defstruct per-buffer-hook
  var
  buffer
  (global t))

(defmethod run-hooks ((hook per-buffer-hook) &rest args)
  (with-slots (var buffer global) hook
    (apply #'run-hooks (variable-value var :buffer buffer) args)
    (when global
      (apply #'run-hooks (variable-value var :global) args))))


;; for test
(defun call-with-global-variable-value (var value function)
  (let* ((editor-variable (get var 'editor-variable))
         (save-value (editor-variable-value editor-variable)))
    (setf (editor-variable-value editor-variable) value)
    (unwind-protect (funcall function)
      (setf (editor-variable-value editor-variable) save-value))))

(defmacro with-global-variable-value ((var value) &body body)
  `(call-with-global-variable-value ',var ,value (lambda () ,@body)))
