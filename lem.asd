(cl:in-package :cl-user)

#-asdf(require :asdf)

(load "inquisitor/inquisitor.asd")

(defpackage :lem-asd
  (:use :cl :asdf))

(in-package :lem-asd)

(defsystem lem
           :serial t
           :components ((:file "fatstring")
                        (:file "package")
                        (:file "wrappers")
                        (:file "key")
                        (:file "header")
                        (:file "util")
                        (:file "hooks")
                        (:file "keymap")
                        (:file "command")
                        (:file "comp")
                        (:file "syntax")
                        (:file "mode")
                        (:file "minibuf")
                        (:file "kill")
                        (:file "point")
                        (:file "region")
                        (:file "buffer")
                        (:file "overlay")
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
                        (:file "lisp-mode")
                        (:file "grep")
                        (:file "lem"))
           :depends-on (:cl-charms
                        :cl-fad
                        #+sbcl :sb-posix
                        :bordeaux-threads
                        :trivial-gray-streams
                        :cl-ppcre
                        :inquisitor
                        :babel))
