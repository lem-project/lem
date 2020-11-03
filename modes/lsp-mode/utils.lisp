(defpackage :lem-lsp-mode/utils
  (:use :cl)
  (:export :get-pid
           :pathname-to-uri
           :find-root-pathname))
(in-package :lem-lsp-mode/utils)

(defun get-pid ()
  #+sbcl
  (sb-posix:getpid)
  #+ccl
  (ccl::getpid)
  #+lispworks
  (progn
    #+win32 (win32:get-current-process-id)
    #-win32 (system::getpid)))

(defun pathname-to-uri (pathname)
  (format nil "file://~A" (namestring pathname)))

(defun find-root-pathname (directory root-test-function)
  (cond ((dolist (file (uiop:directory-files directory))
           (when (funcall root-test-function file)
             (return directory))))
        ((uiop:pathname-equal directory (user-homedir-pathname)) nil)
        ((find-root-directory (uiop:pathname-parent-directory-pathname directory) root-test-function))))
