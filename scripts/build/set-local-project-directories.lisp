;;-*- mode:lisp -*-
(when (uiop:getenv "lem_contrib_prefix")
  (set 'ql:*local-project-directories*
       (list (make-pathname :defaults (uiop:getenv "lem_contrib_prefix"))))
  (setf asdf:*central-registry* nil))
