(cl:in-package :cl-user)

#-asdf(require :asdf)

(defpackage :lem
  (:use :cl))

(defpackage :lem-asd
  (:use :cl :asdf))

(in-package :lem-asd)

(defsystem lem
           :serial t
           :components ((:file "wrappers")
                        (:file "key")
                        (:file "header")
                        (:file "util")
                        (:file "hooks")
                        (:file "keymap")
                        (:file "command")
                        (:file "comp")
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
                        (:file "syntax")
                        (:file "mode")
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
                        :cl-ppcre))
