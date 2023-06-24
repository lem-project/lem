;; usage: sbcl --eval '(ql:quickload :lem-ncurses)' --load build.lisp

#+(and sbcl sb-core-compression)
(sb-ext:save-lisp-and-die "lem"
                          :toplevel 'lem:main
                          :executable t
                          :compression -1)

#+(and sbcl (not sb-core-compression))
(sb-ext:save-lisp-and-die "lem"
                          :toplevel 'lem:main
                          :executable t)

#+ccl
(ccl:save-application "lem"
		      :prepend-kernel t
		      :toplevel-function 'lem:main
		      :error-handler :listener
		      :purify t
		      :application-class 'ccl::application)
