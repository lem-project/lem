(defpackage :lem-lsp-mode/type
  (:use :cl)
  (:import-from :lem-lsp-mode/json
                :json-array-p)
  (:export :lsp-array
           :interface
           :equal-specializer
           :object
           :tuple))
(in-package :lem-lsp-mode/type)

#+sbcl
(sb-ext:lock-package :lem-lsp-mode/type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lsp-array-p (object &optional (element-type '*))
    (typecase object
      (null t)
      (cons
       (if (eq element-type '*)
           (null (cdr (last object)))
           (do ((rest object (cdr rest)))
               ((atom rest)
                (null rest))
             (unless (typep (car rest) element-type)
               (return nil)))))
      (otherwise
       nil))))

(deftype lsp-array (&optional (element-type '*))
  (declare (ignore element-type))
  '(satisfies json-array-p))

(deftype interface (&rest args)
  (declare (ignore args))
  'hash-table)

(deftype equal-specializer (value)
  (declare (ignore value))
  t)

(deftype object (key value)
  (declare (ignore key value))
  `hash-table)

(deftype tuple (&rest types)
  (declare (ignore types))
  'list)
