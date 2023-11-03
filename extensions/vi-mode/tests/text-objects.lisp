(defpackage :lem-vi-mode/tests/text-objects
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/text-objects)

(in-readtable :interpol-syntax)

(deftest word-object
  (with-fake-interface ()
    (with-vi-buffer (#?"abc\n  [ ] def\n")
      (cmd "viw")
      (ok (buf= #?"abc\n<   [ ]>def\n")))))

(deftest double-quoted
  (with-fake-interface ()
    (with-vi-buffer ("[ ]\"foo\" \"bar\"")
      (cmd "va\"")
      (ok (buf= " <\"foo\"[ ]>\"bar\"")))
    (with-vi-buffer (" [\"]foo\" \"bar\"")
      (cmd "va\"")
      (ok (buf= " <\"foo\"[ ]>\"bar\"")))
    (with-vi-buffer (" \"f[o]o\" \"bar\"")
      (cmd "va\"")
      (ok (buf= " <\"foo\"[ ]>\"bar\"")))
    (with-vi-buffer (" \"foo[\"] \"bar\"")
      (cmd "va\"")
      (ok (buf= " <\"foo\"[ ]>\"bar\"")))
    (with-vi-buffer (" \"foo\"[ ]\"bar\"")
      (cmd "va\"")
      ;; NOTE: This behavior is not same as Vim
      (ok (buf= " \"foo\"< \"bar[\"]>")))
    (with-vi-buffer (" \"foo\" \"bar[\"]")
      (cmd "va\"")
      (ok (buf= " \"foo\"< \"bar[\"]>")))))
