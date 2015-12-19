(cl:in-package :cl-user)

#-asdf(require :asdf)

#-uiop(require :uiop)

(let ((dir (merge-pathnames #p"winsize"
                            (make-pathname
                             :directory
                             (pathname-directory *load-truename*)))))
  (unless (probe-file (merge-pathnames #p"winsize.so" dir))
    (uiop:run-program (format nil "cd ~a; sh make.sh" dir))))

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
                        (:file "lisp-mode")
                        ;;(:file "leval-client")
                        (:file "c-mode")
                        (:file "python-mode")
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
                        :babel
                        :usocket
                        :lem-winsize))
