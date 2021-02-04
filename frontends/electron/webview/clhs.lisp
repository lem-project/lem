(defpackage :lem-webview.clhs
  (:use :cl :lem :lem-webview))
(in-package :lem-webview.clhs)

(defparameter *clhs-base-url*
  "http://www.lispworks.com/documentation/HyperSpec/")

(defparameter *clhs-cache-directory*
  (let ((cache-dir
          (uiop:ensure-directory-pathname (merge-pathnames ".lem/" (user-homedir-pathname)))))
    (merge-pathnames #P"clhs/" cache-dir)))

(defparameter *clhs-cache-file*
  (merge-pathnames #P"symbols-map.sexp" *clhs-cache-directory*))

(defmacro with-package-functions (package-designator functions &body body)
  (let ((args (gensym "ARGS")))
    `(flet (,@(loop for fn in functions
                    collect `(,fn (&rest ,args)
                                  (apply
                                   ,(if (and (listp fn) (eq (car fn) 'setf))
                                        `(eval `(function (setf ,(intern ,(string (cadr fn)) ,package-designator))))
                                        `(symbol-function (intern ,(string fn) ,package-designator)))
                                   ,args))))
       ,@body)))

(defun retrieve-url (url)
  (with-package-functions :drakma (http-request)
    (tagbody retry
       (multiple-value-bind (body status)
           (http-request url)
         (unless (= status 200)
           (restart-case
               (error "Failed to retrieve ~S (Code=~A)" url status)
             (retry-request ()
               :report "Retry the request to URL."
               (go retry))))
         (return-from retrieve-url body)))))

(defun clhs-url (path)
  (format nil "~A~A" *clhs-base-url* path))

(defun retrieve-clhs-symbols-map ()
  (lem:maybe-quickload '(:drakma :plump :clss) :silent t)

  (with-package-functions :plump (parse text attribute)
    (with-package-functions :clss (select)
      (let ((body (retrieve-url (clhs-url "Front/X_AllSym.htm"))))
        (map 'list
             (lambda (a)
               (cons (text a)
                     (let ((path (attribute a "href")))
                       ;; Omit "../" and URL fragment
                       (subseq path 3 (position #\# path)))))
             (select "a[rel=definition]" (parse body)))))))

(defun clhs-symbols-map ()
  (if (probe-file *clhs-cache-file*)
      (uiop:read-file-form *clhs-cache-file*)
      (let ((symbols (retrieve-clhs-symbols-map)))
        (ensure-directories-exist *clhs-cache-file*)
        (with-open-file (out *clhs-cache-file*
                             :direction :output
                             :if-does-not-exist :create)
          (prin1 symbols out))
        symbols)))

(defun find-symbol-path (target-symbol)
  (cdr (assoc target-symbol (clhs-symbols-map)
              :test #'string-equal)))

(define-command clhs (name) ("sHyper Spec: ")
  (let ((path (find-symbol-path name)))
    (if path
        (let ((url (clhs-url path)))
          (webview-open url))
        (editor-error "Symbol not found: ~A" name))))
