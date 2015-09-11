(in-package :lem)

(export '(argv pwd files))

(defun argv ()
  #+sbcl
  (cdr sb-ext:*posix-argv*)
  #+ccl
  ccl:*command-line-argument-list*
  #+ecl
  (si:command-args))

(defun pwd ()
  #+sbcl
  (sb-posix:getcwd)
  #+ecl
  (namestring (ext:getcwd))
  #+ccl
  (namestring (cl-user::current-directory)))

(defun files (dirname)
  (mapcar #'namestring (cl-fad:list-directory dirname)))
