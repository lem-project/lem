(in-package :lem-core)

(defun lem-home ()
  (or (uiop:getenv "LEM_HOME")
      (merge-pathnames ".lem/" (user-homedir-pathname))))

(defun lem-logdir-pathname ()
  (merge-pathnames "logs/" (lem-home)))

(defun config-pathname ()
  (merge-pathnames "config.lisp" (lem-home)))

(defun ensure-config-pathname ()
  (ensure-directories-exist (config-pathname)))

(defun config-plist ()
  (let ((pathname (ensure-config-pathname)))
    (if (uiop:file-exists-p pathname)
        (ignore-errors (uiop:read-file-form pathname))
        '())))

(defun config (key &optional default)
  (let ((plist (config-plist)))
    (getf plist key default)))

(defun (setf config) (value key &optional default)
  (declare (ignore default))
  (let ((plist (config-plist)))
    (if (null plist)
        (setf plist (list key value))
        (setf (getf plist key) value))
    (with-open-file (out (ensure-config-pathname)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (pprint plist out)))
  value)
