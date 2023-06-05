(in-package :lem-core)

(defun get-git-hash ()
  (let ((path (asdf:system-relative-pathname :lem ".git/")))
    (when (uiop:directory-exists-p path)
      (uiop:with-current-directory (path)
        (string-trim
         (list #\Newline #\Space)
	 #+sbcl
         (with-output-to-string (stream)
           (uiop:run-program "git rev-parse --short HEAD"
                             :output stream))
	 #-sbcl
	 ""
	 )))))

(defvar *git-revision* (get-git-hash))

(defun lem-git-revision ()
  *git-revision*)

(defun get-version-string ()
  (format nil "lem ~A~@[-~A~] (~A-~A)"
          (asdf:component-version (asdf:find-system :lem))
          *git-revision*
          (machine-type)
          (machine-instance)))
