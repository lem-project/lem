(in-package :lem)

(export '(argv pwd files))

(defun argv ()
  #+sbcl
  (cdr sb-ext:*posix-argv*)
  #-sbcl
  nil)

(defun pwd ()
  #+sbcl
  (sb-posix:getcwd)
  #+ecl
  (namestring (ext:getcwd))
  #+ccl
  (namestring (cl-user::current-directory)))

(defun files (dirname)
  (mapcar #'namestring (cl-fad:list-directory dirname)))
