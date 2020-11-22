(defpackage :lem-lsp-mode/utils
  (:use :cl)
  (:import-from :quri)
  (:import-from :alexandria)
  (:import-from :trivia)
  (:export :get-pid
           :pathname-to-uri
           :uri-to-pathname
           :find-root-pathname
           :do-sequence))
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

(defun uri-to-pathname (uri)
  (pathname (quri:uri-path (quri:uri uri))))

(defun find-root-pathname (directory root-test-function)
  (cond ((dolist (file (uiop:directory-files directory))
           (when (funcall root-test-function file)
             (return directory))))
        ((uiop:pathname-equal directory (user-homedir-pathname)) nil)
        ((find-root-pathname (uiop:pathname-parent-directory-pathname directory) root-test-function))))

(defmacro do-sequence ((var-form sequence) &body body)
  (flet ((parse-var-form (var-form)
           (trivia:ematch var-form
             ((trivia:guard var (symbolp var))
              (values var))
             ((list (trivia:guard element-var (symbolp element-var))
                    (trivia:guard index-var (symbolp index-var)))
              (values element-var index-var)))))
    (multiple-value-bind (element-var index-var)
        (parse-var-form var-form)
      (alexandria:with-gensyms (g-i)
        `(let ,(when index-var `((,g-i 0)))
           (map nil
                (lambda (,element-var)
                  ,(if index-var
                       `(progn
                          (let ((,index-var ,g-i))
                            ,@body)
                          (incf ,g-i))
                       `(progn ,@body)))
                ,sequence))))))
