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
                        (:file "util")
                        (:file "term")
                        (:file "package")
                        (:file "errors")
                        (:file "header")
                        (:file "wide")
                        (:file "key")
                        (:file "point")
                        (:file "overlay")
                        (:file "hooks")
                        (:file "timer")
                        (:file "marker")
                        (:file "keymap")
                        (:file "defcommand")
                        (:file "comp")
                        (:file "buffer")
                        (:file "syntax")
                        (:file "mode")
                        (:file "minibuf")
                        (:file "kill")
                        (:file "region")
                        (:file "io")
                        (:file "buffers")
                        (:file "basic")
                        (:file "command")
                        (:file "screen")
                        (:file "window")
                        (:file "file")
                        (:file "file-command")
                        (:file "word")
                        (:file "search")
                        (:file "info-mode")
                        (:file "sexp")
                        (:file "prog-mode")
                        (:file "listener-mode")
                        (:file "lem")

                        (:file "showparen")
                        (:file "list-buffers")
                        (:file "grep")
                        (:file "lisp-mode")
                        (:file "c-mode")
                        (:file "python-mode")
                        (:file "lua-mode")
                        (:file "dired")
                        (:file "abbrev")

                        (:file "init"))
           :depends-on (:cl-charms
                        :cl-fad
                        :osicat
                        #+sbcl :sb-posix
                        #+sbcl :sb-introspect
                        :swank
                        :trivial-gray-streams
                        :cl-ppcre
                        :inquisitor
                        :babel))
