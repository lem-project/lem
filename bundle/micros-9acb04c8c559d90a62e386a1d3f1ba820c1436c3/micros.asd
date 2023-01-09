(defsystem "micros"
  :depends-on ()
  :version "0.0.0"
  :serial t
  :components ((:file "packages")
               (:module "sbcl"
                :pathname "swank"
                :components ((:file "backend")
                             (:file "source-path-parser")
                             (:file "source-file-cache")
                             (:file "sbcl")
                             (:file "gray")
                             (:file "match")
                             (:file "rpc")))
               (:file "swank")
               (:module "contrib"
                :components ((:file "swank-util")
                             (:file "swank-repl")
                             (:file "swank-c-p-c" :depends-on ("swank-util"))
                             (:file "swank-arglists" :depends-on ("swank-c-p-c"))
                             (:file "swank-fuzzy" :depends-on ("swank-util" "swank-c-p-c"))
                             (:file "swank-fancy-inspector" :depends-on ("swank-util"))
                             (:file "swank-presentations" :depends-on ("swank-repl"))
                             (:file "swank-presentation-streams" :depends-on ("swank-presentations"))
                             (:file "swank-package-fu")
                             (:file "swank-hyperdoc")
                             (:file "swank-sbcl-exts" :depends-on ("swank-arglists"))
                             (:file "swank-mrepl")
                             (:file "swank-trace-dialog")
                             (:file "swank-macrostep")
                             (:file "swank-quicklisp")

                             ;; (:file "swank-asdf")
                             ;; (:file "swank-buffer-streams")
                             ;; (:file "clipboard")
                             ;; (:file "indentation")
                             ;; (:file "listener-hooks" :depends-on ("swank-repl"))
                             ;; (:file "snapshot")
                             ;; (:file "sprof")
                             ))
               (:file "lsp-api")))

(defsystem "micros/client"
  :depends-on ("sb-concurrency"
               "micros"
               "usocket"
               "alexandria"
               "log4cl"
               "async-process")
  :serial t
  :pathname "client"
  :components ((:file "port")
               (:file "main")))
