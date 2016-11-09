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
                        (:file "key")
                        (:file "errors")
                        (:file "header")
                        (:file "wide")
                        (:file "hooks")
                        (:file "screen")
                        (:file "point")
                        (:file "window")
                        (:file "buffer")
                        (:file "buffers")
                        (:file "marker")
                        (:file "overlay")
                        (:file "commandloop")
                        (:file "timer")
                        (:file "input")
                        (:file "syntax")
                        (:file "streams")
                        (:file "kill")
                        (:file "region")
                        (:file "basic")
                        (:file "file")
                        (:file "search")
                        (:file "defcommand")
                        (:file "mode")
                        (:file "keymap")
                        (:file "fundamental-mode")
                        (:file "comp")
                        (:file "minibuf")
                        (:file "info-mode")
                        (:file "lem")

                        (:file "file-command")
                        (:file "window-command")
                        (:file "help-command")
                        (:file "command")

                        (:file "word")
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
                        (:file "go-mode")
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
