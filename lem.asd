(defpackage :lem
  (:use :cl)
  (:export :lem))

(defsystem lem
  :serial t
  :components ((:file "key")
	       (:file "globals")
	       (:file "util")
	       (:file "minibuf")
	       (:file "command")
	       (:file "buffer")
	       (:file "buffers")
	       (:file "bufed")
	       (:file "window")
	       (:file "file")
	       (:file "lem"))
  :depends-on (:cl-ncurses :sb-posix))
