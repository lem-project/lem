(defpackage :lem
  (:use :cl)
  (:export :lem))

(defsystem lem
  :serial t
  :components ((:file "key")
	       (:file "globals")
	       (:file "util")
	       (:file "command")
	       (:file "textbuf")
	       (:file "tblist")
	       (:file "buffer")
	       (:file "window")
	       (:file "minibuf")
	       (:file "file")
	       (:file "lem"))
  :depends-on ("cl-ncurses"))
