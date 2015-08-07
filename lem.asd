(defpackage :lem
  (:use :cl))

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
                        (:file "search")
                        (:file "region")
                        (:file "buffer")
                        (:file "io")
                        (:file "buffers")
                        (:file "bufed")
                        (:file "process")
                        (:file "window")
                        (:file "file")
                        (:file "word")
                        (:file "mode")
                        (:file "sexp")
                        (:file "lisp-mode")
                        (:file "grep")
                        (:file "lem"))
           :depends-on (:cl-charms
                        :cl-fad
                        #+sbcl :sb-posix
                        :bordeaux-threads
                        :trivial-gray-streams))
