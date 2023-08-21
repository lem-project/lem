(defpackage :lem-vi-mode/tests/visual
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem/common/timer
                :with-timer-manager)
  (:import-from :lem-core
                :lem-timer-manager)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/visual)

(in-readtable :interpol-syntax)

(deftest visual-switch
  (with-timer-manager (make-instance 'lem-timer-manager)
    (with-fake-interface ()
      (with-vi-buffer (#?"[a]bc\ndef\n")
        (cmd "v")
        (ok (buf= #?"<[a]>bc\ndef\n"))
        (cmd "l")
        (ok (buf= #?"<a[b]>c\ndef\n"))
        (cmd "v")
        (ok (buf= #?"a[b]c\ndef\n"))
        (cmd "v")
        (ok (buf= #?"a<[b]>c\ndef\n"))
        (cmd "V")
        (ok (buf= #?"<a[b]c>\ndef\n"))
        (cmd "j")
        (ok (buf= #?"<abc\nd[e]f>\n"))
        (cmd "l<C-v>")
        (ok (buf= #?"a<bc>\nd<e[f]>\n"))
        (cmd "v")
        (ok (buf= #?"a<bc\nde[f]>\n"))
        (cmd "v")
        (ok (buf= #?"abc\nde[f]\n"))))))
