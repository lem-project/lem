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

(define-lsp-command "cl-lsp.eval" (arguments)
  (let ((point
          (text-document-position-params-to-point
           (convert-from-json (elt arguments 0)
                              'lsp:text-document-position-params))))
    (let ((result (eval-previous-form point)))
      (notify-show-message lsp:message-type-info
                           result))
    :null))
