(defsystem "lem-opengl"
  :depends-on ("cffi"
	       ;;"cl-charms"
	       "control"
	       "lparallel"
               "trivial-clipboard"
               ;;#+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "lem"

	       #:application
	       #:utility
	       #:text-subsystem
	       ;;#:opengl-immediate
	       #:character-modifier-bits
	       #:uncommon-lisp)
  :serial t
  :components ((:file "package")
	       (:file "ncurses-clone")
               (:file "term")
	       (:file "keys")
	       (:file "impl")
               (:file "sucle")))
