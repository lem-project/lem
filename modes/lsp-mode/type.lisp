(defpackage :lem-lsp-mode/type
  (:use :cl)
  (:import-from :lem-lsp-mode/json
                :json-array-p
                :json-object-p)
  (:export :lsp-array
           :interface
           :equal-specializer
           :object
           :tuple))
(in-package :lem-lsp-mode/type)

#+sbcl
(sb-ext:lock-package :lem-lsp-mode/type)

(deftype lsp-array (&optional (element-type '*))
  (declare (ignore element-type))
  '(satisfies json-array-p))

(deftype interface (&rest args)
  (declare (ignore args))
  '(satisfies json-object-p))

(deftype equal-specializer (value)
  (declare (ignore value))
  t)

(deftype object (key value)
  (declare (ignore key value))
  `(satisfies json-object-p))

(deftype tuple (&rest types)
  (declare (ignore types))
  '(satisfies json-array-p))
