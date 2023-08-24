(defpackage :lem-vi-mode/tests/commands
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/commands)

(in-readtable :interpol-syntax)

(deftest vi-undo
  (with-fake-interface ()
    (with-vi-buffer (#?"[1]\n2\n3\n")
      (cmd "a: Hello!<Esc>")
      (ok (buf= #?"1: Hello[!]\n2\n3\n"))
      (ok (state= :normal))
      (cmd "u")
      (ok (buf= #?"[1]\n2\n3\n"))
      (cmd "<C-r>")
      (ok (buf= #?"1: Hello[!]\n2\n3\n"))
      (cmd "o4: World!<Esc>a<Esc>")
      (ok (buf= #?"1: Hello!\n4: World[!]\n2\n3\n"))
      (cmd "2u")
      (ok (buf= #?"[1]\n2\n3\n")))
    (with-vi-buffer (#?"[1]\n2\n3\n")
      (cmd "a: Hello!<Esc>")
      (ok (buf= #?"1: Hello[!]\n2\n3\n"))
      (cmd "ja: World!<Esc>")
      (ok (buf= #?"1: Hello!\n2: World[!]\n3\n"))
      (cmd "u")
      (ok (buf= #?"1: Hello!\n[2]\n3\n")))))
