(in-package :lem)

(export '(argument-list shell-command))

(defun argument-list ()
  #+sbcl
  (cdr sb-ext:*posix-argv*)
  #+ccl
  ccl:*command-line-argument-list*
  #+ecl
  (si:command-args))

(defun shell-command (command &key input output error-output)
  (uiop:run-program command
                    :input input
                    :output output
                    :error-output error-output
                    :ignore-error-status t))
