(in-package :lem)

(export '(argument-list pwd files shell-command))

(defun argument-list ()
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

(defun shell-command (command &key input output error-output)
  (uiop:run-program command
                    :input input
                    :output output
                    :error-output error-output
                    :ignore-error-status t))
