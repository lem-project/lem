(in-package :lem-language-server)

(defvar *command-table* (make-hash-table :test 'equal))

(defun command-names ()
  (alexandria:hash-table-keys *command-table*))

(defun get-command (name)
  (gethash name *command-table*))

(defun execute-command (name arguments)
  (let ((fn (get-command name)))
    (assert fn) ; TODO: error responseを返す
    (funcall fn arguments)))

(defmacro define-lsp-command (name (arguments) &body body)
  `(setf (gethash ,name *command-table*)
         (lambda (,arguments)
           ,@body)))
