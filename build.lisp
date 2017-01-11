(ql:quickload :lem :silent t)

(defun main ()
  (apply 'lem:lem (uiop:command-line-arguments)))

(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
