(load "repl.asd")
(require :repl)

(repl:server-start "localhost" 53912)

(repl:repl)
