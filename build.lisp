(require :lem)

(if (equal "--debug" (car (lem:argument-list)))
    (defun main ()
      (let ((lem:*debug-p* t))
        (apply 'lem:lem (lem:argument-list))))
    (defun main ()
      (apply 'lem:lem (lem:argument-list))))

#+sb-core-compression
(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t
                          :compression t)

#-sb-core-compression
(sb-ext:save-lisp-and-die "lem"
	                  :toplevel #'main
                          :executable t)
