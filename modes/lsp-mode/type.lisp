(defpackage :lem-lsp-mode/type
  (:use :cl)
  (:import-from :lem-lsp-mode/json
                :json-array-p
                :json-object-p)
  (:export :ts-array
           :ts-interface
           :ts-equal-specializer
           :ts-object
           :ts-tuple))
(in-package :lem-lsp-mode/type)

#+sbcl
(sb-ext:lock-package :lem-lsp-mode/type)

(deftype ts-array (&optional (element-type '*))
  (declare (ignore element-type))
  '(satisfies json-array-p))

(deftype ts-interface (&rest args)
  (declare (ignore args))
  '(satisfies json-object-p))

(deftype ts-equal-specializer (value)
  (declare (ignore value))
  t)

(deftype ts-object (key value)
  (declare (ignore key value))
  `(satisfies json-object-p))

(deftype ts-tuple (&rest types)
  (declare (ignore types))
  '(satisfies json-array-p))
