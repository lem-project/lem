(defpackage :lem
  (:use :cl)
  (:export :lem))

(defsystem lem
  :serial t
  :components ((:file "util")
	       (:file "key")
	       (:file "globals")
	       (:file "textbuf")
	       (:file "buffer")
	       (:file "window")
	       (:file "file")
	       (:file "command")
	       (:file "lem"))
  :depends-on ("cl-ncurses"))
