(require :asdf)
(require :lem)

(defun main ()
  (apply 'lem::lem (lem::argv)))

(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
