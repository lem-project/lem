(defpackage :lem-vi-mode/tests/motion
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/motion)

(named-readtables:in-readtable :interpol-syntax)

(deftest vi-forward-char
  (with-test-buffer (b #?"[a]bcdef\n")
    (with-fake-interface ()
      (with-vi-tests (b)
        (cmd "l")
        (ok (buf= #?"a[b]cdef\n"))
        (cmd "3l")
        (ok (buf= #?"abcd[e]f\n"))
        (cmd "10l")
        (ok (buf= #?"abcde[f]\n"))))))
