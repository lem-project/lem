;; -*- Mode: Lisp; Package: Lem -*-

(ql:quickload :lem)

(defun main ()
  (apply 'lem::lem (lem:argument-list)))

(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
