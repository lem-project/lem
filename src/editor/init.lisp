(in-package :lem)

(defun load-init-file ()
  (flet ((test (path)
	   (when (cl-fad:file-exists-p path)
	     (load path)
	     (message "Load file: ~a" path)
	     t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(add-hook *after-init-hook* 'load-init-file)

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)

(setf asdf:*central-registry*
      (union (mapcar #'pathname
                     (mapcar #'directory-namestring
                             (directory
                              (merge-pathnames "**/*.asd"
                                               (merge-pathnames "tools/"
                                                                (asdf:system-source-directory :lem))))))
             asdf:*central-registry*
             :test #'equal))
