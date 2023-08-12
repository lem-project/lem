(defpackage :lem-translator
  (:use :cl :lem))

(in-package :lem-translator)

(defclass service ()
  ((auth-key
    :initarg :auth-key
    :reader service-key
    :type (or string null))
   (api-url
    :initarg :api-url
    :accessor service-api-url
    :type string)))

(defclass deepl (service) ())
(defgeneric translate-string (service &key from to string))

(defgeneric translate-region (service &key from to
                                           region-start region-end
                                           string replace))
