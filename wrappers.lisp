(in-package :lem)

(export '(argv pwd files shell-command))

(defun argv ()
  #+sbcl
  (cdr sb-ext:*posix-argv*)
  #-sbcl
  nil)

(defun bytes-to-string (bytes)
  #+sbcl
  (sb-ext:octets-to-string bytes)
  #-sbcl
  (code-char (aref bytes 0)))

(defun pwd ()
  #+sbcl
  (sb-posix:getcwd)
  #+ecl
  (namestring (ext:getcwd))
  #+ccl
  (namestring (cl-user::current-directory)))

(defun files (dirname)
  (mapcar #'namestring (cl-fad:list-directory dirname)))

(defun shell-command (str &key output input)
  #+sbcl
  (sb-ext:run-program "/bin/sh" (list "-c" str)
                      :output output
                      :input input)
  #+ecl
  (destructuring-bind (cmd &rest args)
      (split-string str #\space)
    (let ((s (ext:run-program cmd args)))
      (loop for x = (read s nil nil)
        while x
        do (print x output))))
  #+ccl
  (cl-user::run-program "/bin/sh" (list "-c" str)
                        :output output 
                        :input input))
