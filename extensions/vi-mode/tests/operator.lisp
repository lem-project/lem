(defpackage :lem-vi-mode/tests/operator
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/operator)

(in-readtable :interpol-syntax)

(deftest vi-delete
  (with-fake-interface ()
    (with-vi-buffer (#?"a[b]c\ndef\nghi\njkl\n")
      (cmd "dd")
      (ok (buf= #?"d[e]f\nghi\njkl\n"))
      (cmd "dd")
      (ok (buf= #?"g[h]i\njkl\n"))
      (cmd "p")
      (ok (buf= #?"ghi\n[d]ef\njkl\n"))
      (cmd "2dd")
      (ok (buf= #?"ghi\n[]")))))

(deftest vi-join-line
  (with-fake-interface ()
    (lem:window-set-size (lem:current-window) 5 24)
    (with-vi-buffer (#?"[a]bcdefgh\nijklmn\n")
      (cmd "J")
      (ok (buf= #?"abcdefgh[ ]ijklmn\n")))))
