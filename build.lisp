(load (make-pathname :defaults *load-pathname* :name "lem" :type "asd"))
(ql:quickload :lem :silent t)

#+sbcl
(loop for i in (remove "sb-mpfr"
                       (loop with result
                         for i in (directory (format nil "~A/contrib/*.*" (sb-posix:getenv "SBCL_HOME")))
                         do (pushnew (pathname-name i) result :test 'equal)
                         finally (return (nreverse result))) :test 'string-equal)
  do (require i))

(if (equal "--debug" (car (uiop:command-line-arguments)))
    (defun main ()
      (let ((lem:*debug-p* t))
        (apply 'lem:lem (uiop:command-line-arguments))))
    (defun main ()
      (apply 'lem:lem (uiop:command-line-arguments))))

#+sb-core-compression
(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t
                          :compression t)

#-sb-core-compression
(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
