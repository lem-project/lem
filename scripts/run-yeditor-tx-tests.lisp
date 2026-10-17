(load ".qlot/setup.lisp")

(require :asdf)

(asdf:load-asd (merge-pathnames "yeditor/tx/yeditor-tx.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/tx/tx-tests.asd" (uiop:getcwd)))
(asdf:load-system :yeditor-tx)

(asdf:test-system :yeditor-tx-tests)
(quit)