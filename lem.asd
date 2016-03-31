(cl:in-package :cl-user)

#-asdf(require :asdf)

#-uiop(require :uiop)

(defpackage :lem-asd
  (:use :cl :asdf))

(in-package :lem-asd)

(defsystem lem
           :serial t
           :components ((:file "fatstring")
                        (:file "queue")
			(:file "winsize")
                        (:file "package")
                        (:file "wrappers")
                        (:file "errors")
                        (:file "header")
                        (:file "util")
                        (:file "wide")
                        (:file "key")
                        (:file "point")
                        (:file "misc")
                        (:file "keymap")
                        (:file "command")
                        (:file "comp")
                        (:file "buffer")
                        (:file "syntax")
                        (:file "mode")
                        (:file "minibuf")
                        (:file "kill")
                        (:file "region")
                        (:file "io")
                        (:file "buffers")
                        (:file "bufed")
                        (:file "process")
                        (:file "window")
                        (:file "file")
                        (:file "word")
                        (:file "search")
                        (:file "info-mode")
                        (:file "sexp")
                        (:file "grep")
                        (:file "prog-mode")
                        (:file "listener-mode")
                        (:file "lisp-mode")
                        (:file "c-mode")
                        (:file "python-mode")
                        (:file "lua-mode")
                        (:file "dired")
                        (:file "lem"))
           :depends-on (:cl-charms
                        :cl-fad
                        :osicat
                        #+sbcl :sb-posix
                        #+sbcl :sb-introspect
                        :trivial-gray-streams
                        :cl-ppcre
                        :inquisitor
                        :babel))
