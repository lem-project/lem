(in-package :lem-language-server)

(defvar *method-classes* '())

(defgeneric call (lsp-method json))

(defclass lsp-method ()
  ((name :initarg :name
         :reader lsp-method-name)))

(defmacro define-request ((class-name method-name)
                          (&optional (params (gensym "params")) (params-type nil params-type-p))
                          &body body)
  (with-unique-names (instance json)
    `(progn
       (defclass ,class-name (lsp-method)
         ()
         (:default-initargs :name ,method-name))
       (defmethod call ((,instance ,class-name) ,json)
         (let ((,params ,(if params-type-p
                             `(json:coerce-json ,json ',params-type)
                             json)))
           ,@(unless params-type-p `((declare (ignore ,params))))
           ,@body))
       (pushnew ',class-name *method-classes*))))
