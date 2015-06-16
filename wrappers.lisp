(in-package :lem)

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
