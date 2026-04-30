(in-package :lem-core)

(defun get-git-hash ()
  "Return lem's git hash."
  ;; Skip git operations during Nix build
  #-nix-build
  (let ((path (asdf:system-relative-pathname :lem ".git/")))
    (when (uiop:directory-exists-p path)
      (let ((repo-root (uiop:pathname-directory-pathname path)))
        (uiop:with-current-directory (repo-root)
          (handler-case
              (string-trim
               (list #\Newline #\Space)
               #+sbcl
               (with-output-to-string (stream)
                 (uiop:run-program "git rev-parse --short HEAD"
                                   :output stream
                                   :ignore-error-status nil))
               #-sbcl
               "")
            (error () nil)))))))

(defvar *git-revision* (get-git-hash)
  "Stores lem's git revision; this is treated as a cache.")

(defun lem-git-revision ()
  "Return lem's git revision in string."
  *git-revision*)

(defun get-version-string ()
  "Return the version number of this version of Lem."
  (format nil "lem ~A~@[-~A~] (~A-~A)"
          (asdf:component-version (asdf:find-system :lem))
          *git-revision*
          (machine-type)
          (machine-instance)))
