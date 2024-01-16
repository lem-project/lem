(in-package :lem-translator)

(defclass lingva (service)
  ((language-list
    :initarg :language-list
    :accessor lingva-language-list
    :type list)))

(defun %get-lingva-token (root-url)
  "Update the translation token."
  (let* ((body (dexador:get root-url))
         (position (cl-ppcre:scan "buildManifest" body))
         (token (subseq body (- position 23) (- position 2))))
    token))

;;(setf lin (make-instance 'lingva :api-url
;;                         "https://translate.plausibility.cloud/_next/data/~a/~a/~a/~a.json")

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
                        (do-urlencode:urlencode string))))))))