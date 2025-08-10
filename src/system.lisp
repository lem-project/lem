(in-package :lem-core)

(defparameter *deployed* nil)

(defun lem-relative-pathname (pathname)
  (when *deployed*
    (truename (merge-pathnames pathname
			       #+sbcl
                               (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))
			       #-sbcl
                               (uiop:pathname-directory-pathname (uiop/os:getcwd))))))

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

(defun open-external-file (pathname)
  #+linux
  (uiop:launch-program (list "xdg-open" (namestring pathname)))
  #+darwin
  (uiop:launch-program (list "open" (namestring pathname)))
  #+windows
  (uiop:launch-program (list "explorer" (namestring pathname)) :ignore-error-status t))

;;;; PATH

(let ((cache ()))
  (defun exec-path (&optional refresh)
    "Return a list pathname of executable search path. 

Parameters:
+ REFRESH: the return value of `exec-path' is by default
  cached, if needed to refresh it, set REFRESH to non-nil

Note:
+ For macOS, it should be composed from: 
  + /etc/paths
  + /etc/paths.d/*
+ For linux, it should be $PATH variable directly. "
    (if (and (not refresh) cache)
        cache
        (setf cache
              (mapcar 
               #'uiop:ensure-directory-pathname
               #+darwin
               (append (reduce #'append
                               (mapcar #'uiop:read-file-lines
                                       (uiop:directory-files #P"/etc/paths.d/")))
                       (uiop:read-file-lines #P"/etc/paths")
                       (when (uiop:directory-exists-p "/opt/homebrew/bin/")
                         (list "/opt/homebrew/bin/"))
                       (delete-if (lambda (str) (zerop (length str)))
                                  (split-sequence:split-sequence #\: (uiop:getenv "PATH"))))
               #+linux
               (delete-if (lambda (str) (zerop (length str)))
                          (split-sequence:split-sequence #\: (uiop:getenv "PATH")))
               #-(or darwin linux)
               (error "Don't know how to get PATH env. Please hack me! "))))))
