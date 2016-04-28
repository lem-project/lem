(cl:in-package :cl-user)

#-asdf(require :asdf)

#-uiop(require :uiop)

(defpackage :lem-asd
  (:use :cl :asdf))

(in-package :lem-asd)

(defsystem lem
           :serial t
           :components ((:file "fatstring")
                        (:file "util")
                        (:file "term")
                        (:file "package")
                        (:file "errors")
                        (:file "hooks")
                        (:file "header")
                        (:file "screen")
                        (:file "wide")
                        (:file "key")
                        (:file "point")
                        (:file "overlay")
                        (:file "timer")
                        (:file "marker")
                        (:file "keymap")
                        (:file "comp")
                        (:file "buffer")
                        (:file "syntax")
                        (:file "defcommand")
                        (:file "mode")
                        (:file "minibuf")
                        (:file "kill")
                        (:file "region")
                        (:file "io")
                        (:file "buffers")
                        (:file "basic")
                        (:file "input")
                        (:file "window")
                        (:file "file")
                        (:file "search")
                        (:file "info-mode")
                        (:file "lem")

                        (:file "file-command")
                        (:file "window-command")
                        (:file "help-command")
                        (:file "word")
                        (:file "command")

                        (:file "sexp")

                        (:file "prog-mode")
                        (:file "listener-mode")

                        (:file "kbdmacro")
                        (:file "isearch")
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
