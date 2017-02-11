(in-package :lem-base)

(export '(define-editor-variable
          value))

(defstruct editor-variable
  value
  documentation
  local-indicator)

(defmacro define-editor-variable (var &optional value documentation)
  (check-type var symbol)
  `(unless (get ',var 'editor-variable)
     (defvar ,var)
     (setf (get ',var 'editor-variable)
           (make-editor-variable :value ,value
                                 :documentation ,documentation
                                 :local-indicator (gensym ,(string var))))
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

(defun value (symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (get-bvar (editor-variable-local-indicator var)
                   :buffer buffer
                   :default (editor-variable-value var))))
      ((:buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (get-bvar (editor-variable-local-indicator var)
                   :buffer buffer)))
      ((:global)
       (editor-variable-value var)))))

(defun (setf value) (value symbol &optional (kind :default) (where nil wherep))
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default :buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (setf (get-bvar (editor-variable-local-indicator var)
                         :buffer buffer)
               value)))
      ((:global)
       (setf (editor-variable-value var) value)))))
