(defpackage :lem/thingatp
  (:use :cl :lem)
  (:export 
   :urlp
   :url
   :pathp
   :path))

;; URL
(defun urlp (thing)
  (and
   (stringp thing)
   (numberp
    (ppcre:scan 
     "(https:\/\/www\.|http:\/\/www\.|https:\/\/|http:\/\/)[a-zA-Z0-9]{2,}(\.[a-zA-Z0-9]{2,})(\.[a-zA-Z0-9]{2,})?" thing))))

(deftype url ()
  `(satisfies urlp))

;; PATH
(defun pathp (thing)
  (and (ppcre:scan "^(.*)\/([^\/]*)$" thing)
       (or (uiop:directory-exists-p thing) 
           (uiop:file-exists-p thing))))

(deftype path ()
  `(satisfies pathp))

(lem:define-command open-at-point () ()
  (let ((thing (lem:symbol-string-at-point (lem:current-point))))
    (typecase thing
      #+unix
      (url (uiop:run-program 
	    (format nil "open ~a" thing)))
      (path
       (lem:find-file (pathname thing)))
      (t
       (lem/language-mode::find-definitions)))))
