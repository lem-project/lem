;#!/bin/sh
;#|-*- mode:lisp -*-|#
;#|
;exec ros -Q -- $0 "$@"
;|#

#|

A Roswell script to open the HyperSpec page of a specified symbol in the default browser.

Usage
-----

    $ clhs [SYMBOL]


Installation
------------

Just download this script, give execute permission, and move to somewhere your shell can find it (assuming `~/.roswell/bin/` is in $PATH in the following example).

    $ wget https://gist.githubusercontent.com/fukamachi/3510ea1609c1b52830c2/raw/clhs.ros -O clhs
    $ chmod u+x clhs
    $ mv clhs ~/.roswell/bin

You may want to `ros build` for creating an executable file for fast execution.

    $ ros build clhs.ros
    $ mv clhs ~/.roswell/bin


Environment variables
---------------------

    CLHS_BASE_URL:
      The base URL of HyperSpec. The default is LispWorks'.

    CLHS_OPEN_COMMAND:
      Command name to open an URL with the default browser.
      The default value is 'open' for Mac and 'xdg-open' for Linux.


Copyright
---------

Copyright (c) 2015 Eitaro Fukamachi, Masatoshi Sano


LICENSE
-------

This script is licensed under the MIT License.

|#

;; Loading dependencies
;(unless (find-package :uiop)
;  (ql:quickload '(:uiop) :silent t))

(defpackage :lem-slime.clhs
  (:use :cl)
  (:export :main))

(in-package :lem-slime.clhs)

;;
;; Special variables

(defparameter *clhs-base-url*
  (or (uiop:getenv "CLHS_BASE_URL")
      "http://www.lispworks.com/documentation/HyperSpec/"))

(defparameter *clhs-cache-directory*
  (let ((cache-dir
          (uiop:ensure-directory-pathname (uiop:getenv "XDG_CACHE_HOME")
                                          (merge-pathnames ".cache/" (user-homedir-pathname)))))
    (merge-pathnames #P"clhs/" cache-dir)))

(defparameter *clhs-cache-file*
  (merge-pathnames #P"symbols-map.sexp" *clhs-cache-directory*))

(defparameter *open-command*
  (or (uiop:getenv "CLHS_OPEN_COMMAND")
      #+darwin "open"
      #+linux  "xdg-open"
      #+(or windows win32) "explorer"
      #-(or darwin linux windows win32)
      (error "CLHS_OPEN_COMMAND is not set.")))


;;
;; Utilities

(defun terminate (code &optional message &rest args)
  (when message
    (format *error-output* "~&~A~%"
            (apply #'format nil (princ-to-string message) args)))
  (uiop:quit code))

;; Copied from Qlot
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


;;
;; Here we go!

(defun clhs-url (path)
  (format nil "~A~A" *clhs-base-url* path))

(defun retrieve-clhs-symbols-map ()
  (ql:quickload '(:drakma :plump :clss) :silent t)

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

(defun main (&optional target-symbol &rest argv)
  (declare (ignore argv))
  (unless target-symbol
    (terminate -1 "Usage: clhs [SYMBOL]"))

  (let ((path (find-symbol-path target-symbol)))
    (if path
        (let ((url (clhs-url path)))
          (format t "~&Opening ~S~%" url)
          (uiop:run-program `(,*open-command* ,url) :ignore-error-status t))
        (terminate -1 "Symbol not found: ~A" target-symbol))))
