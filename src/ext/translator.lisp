(defpackage :lem/translator
  (:use :cl :lem))


(in-package :lem/translator)

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

(defclass lingva (service)
  ((language-list
    :initarg :language-list
    :accessor lingva-language-list
    :type list)))

(defgeneric translate-string (service &key from to string))

(defgeneric translate-region (service &key from to
                                           region-start region-end
                                           string replace))

(declaim (ftype (function ((unsigned-byte 8)) character) octet-to-ascii))
(defun octet-to-ascii (octet)
  (aref (babel:octets-to-string (make-array '(1)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element octet)
                          :encoding :ASCII) 0))

(declaim (type (array (unsigned-byte 8) (4)) +extra-unreserved-octets+))
(defvar +extra-unreserved-octets+
  (make-array '(4) :element-type '(unsigned-byte 8)
              :initial-contents #(#x2D #x2E #x5F #x7E)))

(declaim (ftype (function ((unsigned-byte 8)) boolean) unreserved-octet-p))
(defun unreserved-octet-p (o)
  (or (<= #x30 o #x39) ; #\0 to #\9
      (<= #x41 o #x5A) ; #\A to #\Z
      (<= #x61 o #x7A) ; #\a to #\z
      (if (find o +extra-unreserved-octets+ :test #'=) t nil)))

(define-condition urlencode-malformed-string (error)
  ((string :initarg :string :reader urlencode-malformed-string-string))
  (:report (lambda (c stream)
             (format stream "The string ~s is not a valid urlencoded string."
                     (urlencode-malformed-string-string c)))))

(declaim (ftype (function (simple-string
                           &key (:queryp boolean))
                          simple-string)
                urlencode))

(defun urlencode (string &key (queryp nil))
  (loop
    with octets of-type (simple-array (unsigned-byte 8) (*)) = (babel:string-to-octets string :encoding :UTF-8)
    with result = (make-string (* 3 (length octets)))
    for o across octets
    with i of-type fixnum = 0
    do (flet ((push-char (c)
                (setf (aref result i) c)
                (incf i)))
         (cond ((unreserved-octet-p o)
                (push-char (octet-to-ascii o)))
               ((and queryp (= o #x20))
                (push-char #\+))
               (t (let ((h (digit-char (ash (dpb 0 (byte 4 0) o) -4) 16))
                        (l (digit-char (dpb 0 (byte 4 4) o) 16)))
                    (push-char #\%)
                    (push-char h)
                    (push-char l)))))
    finally (return (subseq result 0 i))))

(defun %get-lingva-token (root-url)
  "Update the translation token."
  (let* ((body (dexador:get root-url))
         (position (cl-ppcre:scan "buildManifest" body))
         (token (subseq body (- position 23) (- position 2))))
    token))


(setf lin (make-instance 'lingva :api-url
                         "https://translate.plausibility.cloud/_next/data/~a/~a/~a/~a.json")

      (defmethod translate-string ((service lingva) &key from to string)
		 (let* ((root-url (str:split "/" (service-api-url service)))
			(token (%get-lingva-token
				(concatenate 'String
					     (first root-url)
					     "//"
					     (third root-url)))))
		   (gethash "translation"
			    (gethash
			     "pageProps"
			     (yason:parse
			      (dex:get
			       (format nil (service-api-url service)
				       token
				       from to
				       (urlencode string))))))))
