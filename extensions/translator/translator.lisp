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

(defvar *translation-service* nil)

(defgeneric translate-string (service &key from to string))

(defgeneric translate-region (service &key from to
                                           region-start region-end
                                           replace))

(defgeneric translate-output (output source-text final-text))

(defmethod translate-output ((buffer buffer) source-text final-text)
  (with-open-stream (stream (make-buffer-output-stream (buffer-end-point buffer)))
    (fresh-line stream)
    (format stream "---------- source ----------~%")
    (format stream "~A~%" source-text)
    (format stream "---------- target ----------~%")
    (format stream "~A~2%" final-text)))

(define-command translator-region (start end &optional is-replace) ("r" "P")
  (if *translation-service*
      (translate-region *translation-service*
                        :region-start start
                        :region-end end
                        :replace is-replace)
      (message
       "No translation service selected, please fill the variable *lem-translator:translation-service*")))
