(load ".qlot/setup.lisp")

(ql:quickload :rove)

(asdf:load-asd (merge-pathnames "yeditor/tx/yeditor-tx.asd" (uiop:getcwd)))
(asdf:load-system :yeditor-tx)

(load "yeditor/tx/tx-test.lisp")

(rove:run :lem-tests/tx-test)
(quit)
