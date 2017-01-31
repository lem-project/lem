(in-package :lem-base)

(export '(define-editor-variable value))

(defstruct editor-variable
  value
  documentation
  local-indicator)

(defmacro define-editor-variable (var &optional value documentation)
  (check-type var symbol)
  `(progn
     (setf (get ',var 'editor-variable)
           (make-editor-variable :value ,value
                                 :documentation ,documentation
                                 :local-indicator (gensym ,(string var))))
     t))

(defun check-editor-variable (symbol)
  (unless (editor-variable-p (get symbol 'editor-variable))
    (error "~A is not editor variable" symbol)))

(defun value (symbol &optional (buffer (current-buffer)))
  (check-type buffer (or null buffer))
  (check-editor-variable symbol)
  (let ((var (get symbol 'editor-variable)))
    (if (null buffer)
        (editor-variable-value var)
        (get-bvar (editor-variable-local-indicator var)
                  :buffer buffer :default (editor-variable-value var)))))

(defun (setf value) (value symbol &optional (buffer (current-buffer)))
  (check-type buffer (or buffer null))
  (check-editor-variable symbol)
  (let ((var (get symbol 'editor-variable)))
    (if (null buffer)
        (setf (editor-variable-value var) value)
        (setf (get-bvar (editor-variable-local-indicator var)
                        :buffer buffer)
              value))))
