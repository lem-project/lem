(ql:quickload :lem)

(if (equal "--debug" (car (lem:argument-list)))
    (defun main ()
      (let ((lem:*debug-p* t))
        (apply 'lem:lem (lem:argument-list))))
    (defun main ()
      (apply 'lem:lem (lem:argument-list))))

(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
