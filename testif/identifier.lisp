(in-package :testif)

(defclass identifier ()
  ((name :initarg :name
         :type string
         :reader identifier-name)
   (package :initarg :package
            :type package
            :reader identifier-package)))

(defun identifier-content (identifier)
  (identifier-name identifier))

(defun identifier-equal (id1 id2)
  (and (string= (identifier-name id1) (identifier-name id2))
       (eq (identifier-package id1) (identifier-package id2))))

(defun ensure-identifier (name)
  (etypecase name
    (identifier name)
    (string (make-instance 'identifier :name name :package *package*))
    (symbol (make-instance 'identifier :name (string name) :package (symbol-package name)))))
