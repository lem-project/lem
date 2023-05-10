(in-package :lem)

(defparameter *deployed* nil)

(defun lem-relative-pathname (pathname)
  (when *deployed*
    (merge-pathnames pathname (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))))

(defun get-pid ()
  #+sbcl
  (sb-posix:getpid)
  #+ccl
  (ccl::getpid)
  #+lispworks
  (progn
    #+win32 (win32:get-current-process-id)
    #-win32 (system::getpid)))

(defun exist-program-p (program)
  (let ((status
          (nth-value 2
                     (uiop:run-program (list "which" program)
                                       :ignore-error-status t))))
    (= status 0)))
