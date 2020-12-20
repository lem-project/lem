(defpackage :lem-lsp-utils/type
  (:use :cl)
  (:import-from :cl-package-locks)
  (:import-from :lem-lsp-utils/json
                :json-array-p
                :json-object-p)
  (:export :ts-array
           :ts-interface
           :ts-equal-specializer
           :ts-object
           :ts-tuple
           :ts-boolean))
(in-package :lem-lsp-utils/type)

(cl-package-locks:lock-package :lem-lsp-utils/type)

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
  '(satisfies json-object-p))

(deftype ts-tuple (&rest types)
  (declare (ignore types))
  '(satisfies json-array-p))

(deftype ts-boolean ()
  '(satisfies json-boolean-p))
