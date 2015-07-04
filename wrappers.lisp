(in-package :lem)

(export '(argv pwd files shell-command))

(defun argv ()
  (cdr sb-ext:*posix-argv*))

(defun bytes-to-string (bytes)
  (sb-ext:octets-to-string bytes))

(defun pwd ()
  (sb-posix:getcwd))

(defun files (dirname)
  (mapcar #'namestring
    (directory
     (make-pathname
      :name :wild
      :type :wild
      :defaults dirname))))

(defun shell-command (str &key output input)
  (sb-ext:run-program "/bin/sh" (list "-c" str)
    :output output
    :input input))
