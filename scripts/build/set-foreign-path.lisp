(in-package :lem)

(ignore-errors (cffi:close-foreign-library 'async-process::async-process))
(setf
 cffi:*foreign-library-directories*
 (remove-if (lambda (x)
              (and (pathnamep x)
                   (find "async-process" (pathname-directory x) :test 'string=)))
            cffi:*foreign-library-directories*))

(defun init-foreign-config ()
  #+sbcl
  (let* ((bin (make-pathname :defaults (merge-pathnames (first sb-ext:*posix-argv*)) :type nil :name nil))
         (lib (merge-pathnames "../lib/" bin)))
    (push bin cffi:*foreign-library-directories*)
    (push lib cffi:*foreign-library-directories*)
    (cffi:load-foreign-library 'async-process::async-process)))
    

(add-hook *before-init-hook* 'init-foreign-config)