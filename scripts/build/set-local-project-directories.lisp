(in-package :lem)

(defun init-path-config ()
  (push (merge-pathnames "local-projects/" (lem-home)) roswell:*local-project-directories*)
  (setf ql:*quicklisp-home* (merge-pathnames "quicklisp/" (lem-home))))

(add-hook *before-init-hook* 'init-path-config)

(when (uiop:getenv "lem_contrib_prefix")
  (setf roswell:*local-project-directories*
        (remove-if-not
         (lambda (x)
           (let ((a (last (pathname-directory x) 2)))
             (and (equal (second a) "site-lisp")
                  (equal (asdf:component-version (asdf:find-system :lem))
                         (first a)))))
         (mapcar (lambda (x)
                   (make-pathname :defaults x :name nil :type nil))
                 (directory (merge-pathnames
                             "**/system-index.txt"
                             (make-pathname :defaults (uiop:getenv "lem_contrib_prefix")))))))
  (set 'ql:*local-project-directories* nil)
  (setf asdf:*central-registry* nil)
  (asdf:clear-source-registry)
  (setf roswell.util::*local-project-cache* nil))
