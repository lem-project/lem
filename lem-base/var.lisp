(in-package :lem-base)

(export '(define-editor-variable
          clear-editor-local-variables
          variable-value
          variable-documentation))

(defvar *editor-variables* '())

(defstruct editor-variable
  value
  documentation
  local-indicator
  change-value-hook)

(defun clear-editor-local-variables (buffer)
  (dolist (symbol *editor-variables*)
    (buffer-unbound buffer
                    (editor-variable-local-indicator
                     (get symbol 'editor-variable)))))

(defmacro define-editor-variable (var &optional value documentation change-value-hook)
  (check-type var symbol)
  `(unless (get ',var 'editor-variable)
     (defvar ,var)
     (pushnew ',var *editor-variables*)
     (setf (get ',var 'editor-variable)
           (make-editor-variable :value ,value
                                 :documentation ,documentation
                                 :local-indicator (gensym ,(string var))
                                 :change-value-hook ,change-value-hook))
     t))

(defun editor-variable-error (symbol)
  (error "~A is not editor variable" symbol))

(defun check-editor-variable (symbol)
  (unless (editor-variable-p (get symbol 'editor-variable))
    (editor-variable-error symbol)))

(defun ensure-buffer (where)
  (if (pointp where)
      (point-buffer where)
      (progn
        (check-type where buffer)
        where)))

(defun variable-value (symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (buffer-value buffer
                       (editor-variable-local-indicator var)
                       (editor-variable-value var))))
      ((:buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (buffer-value buffer
                       (editor-variable-local-indicator var))))
      ((:global)
       (editor-variable-value var)))))

(defun (setf variable-value) (value symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default :buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (setf (buffer-value buffer
                             (editor-variable-local-indicator var))
               value)))
      ((:global)
       (let ((fn (editor-variable-change-value-hook var)))
         (when fn
           (funcall fn value)))
       (setf (editor-variable-value var) value)))))

(defun variable-documentation (symbol)
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (editor-variable-documentation var)))
