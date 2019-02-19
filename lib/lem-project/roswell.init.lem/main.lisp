(uiop/package:define-package :roswell.init.lem/main
                             (:nicknames :roswell.init.lem) (:use :cl)
                             (:shadow) (:export) (:intern))
(in-package :roswell.init.lem/main)
;;don't edit above
(defun lem (&rest argv)
  (unless (= (length argv) 2)
    (write-line "usage: ros init lem [path]")
    (uiop:quit 1))
  (let ((path (second argv)))
    (when path
      (lem-project:make-project
       (merge-pathnames path
                        (probe-file "./"))))))
