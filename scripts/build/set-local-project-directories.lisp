(in-package :lem)

(defun init-path-config ()
  (setf roswell:*local-project-directories* nil)
  #+sbcl
  (let* ((bin (make-pathname :defaults (merge-pathnames (first sb-ext:*posix-argv*)) :type nil :name nil))
         (site-lisp (merge-pathnames (format nil "../share/lem/~A/site-lisp/" (asdf:component-version (asdf:find-system :lem)))
                                     bin)))
    (push (uiop:truenamize site-lisp) roswell:*local-project-directories*))
  (setf ql:*local-project-directories* nil)
  (setf asdf:*central-registry* nil)
  (asdf:clear-source-registry)
  (setf roswell.util::*local-project-cache* nil)
  (push (merge-pathnames "local-projects/" (lem-home)) roswell:*local-project-directories*)
  (roswell.util::local-project-build-hash :rebuild t)
  (setf ql:*quicklisp-home* (merge-pathnames "quicklisp/" (lem-home))))

(add-hook *before-init-hook* 'init-path-config)

