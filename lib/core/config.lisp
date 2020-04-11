(in-package :lem)

(defun lem-home ()
  (or (uiop:getenv "LEM_HOME")
      (merge-pathnames ".lem/" (user-homedir-pathname))))
