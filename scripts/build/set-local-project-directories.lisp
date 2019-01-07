;;-*- mode:lisp -*-
(when (uiop:getenv "lem_contrib_prefix")
  (set 'ql:*local-project-directories*
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
  (setf asdf:*central-registry* nil))
