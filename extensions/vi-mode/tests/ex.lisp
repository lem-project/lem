(defpackage :lem-vi-mode/tests/ex
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/ex)

(in-readtable :interpol-syntax)

(deftest ex-substitute
  (with-fake-interface ()
    (testing "visual-char"
      (with-vi-buffer (#?"pen pineapple <apple pen\nap[p]>le juice\npineapple cake\n")
        (ex-cmd "'<,'>s/apple/grape")
        (ok (text= #?"pen pinegrape apple pen\ngrape juice\npineapple cake\n"))))
    (testing "visual-line"
      (with-vi-buffer (#?"[p]en pineapple apple pen\napple juice\npineapple cake\n")
        (cmd "Vj")
        (ex-cmd "'<,'>s/apple/grape")
        (ok (text= #?"pen pinegrape apple pen\ngrape juice\npineapple cake\n"))))
    (testing "'g' flag"
      (with-vi-buffer (#?"pen pineapple <apple pen\nap[p]>le juice\npineapple cake\n")
        (ex-cmd "'<,'>s/apple/grape/g")
        (ok (text= #?"pen pinegrape grape pen\ngrape juice\npineapple cake\n"))))))
