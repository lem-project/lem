(cl:in-package :cl-user)

#-asdf(require :asdf)

(defpackage :lem-asd
  (:use :cl :asdf))

(in-package :lem-asd)

(defsystem lem
           :serial t
           :components ((:file "fatstring")
                        (:file "queue")
                        (:file "package")
                        (:file "wrappers")
                        (:file "header")
                        (:file "util")
                        (:file "key")
                        (:file "point")
                        (:file "hooks")
                        (:file "keymap")
                        (:file "command")
                        (:file "comp")
                        (:file "overlay")
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
                        (:file "lisp-mode")
                        (:file "lserver-mode")
                        (:file "grep")
                        (:file "lem"))
           :depends-on (:cl-charms
                        :cl-fad
                        #+sbcl :sb-posix
                        :bordeaux-threads
                        :swank-client
                        :trivial-gray-streams
                        :cl-ppcre
                        :inquisitor
                        :babel))
