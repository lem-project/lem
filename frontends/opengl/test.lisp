(in-package :lem-sucle)
(defun start-lem ()
  (let ((lem::*in-the-editor* nil))
    (lem:main '("/home/imac/Documents/common-lisp/sucle.lisp"))))
