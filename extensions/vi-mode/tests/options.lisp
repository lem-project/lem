(defpackage :lem-vi-mode/tests/options
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/options
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:shadowing-import-from :lem-vi-mode/tests/utils
                          :with-current-buffer))
(in-package :lem-vi-mode/tests/options)

(deftest get-option
  (ok (typep (get-option "number") 'vi-option)
      "Can get a global option")
  (ok (typep (get-option "iskeyword") 'vi-option)
      "Can get a buffer-local option")
  (let ((isk (vi-option-value "iskeyword")))
    (setf (vi-option-value "iskeyword") '("@" "_"))
    (ok (equalp (vi-option-value "iskeyword") '("@" "_"))
        "Can set a buffer-local option")
    (with-fake-interface ()
      (with-test-buffer (buf "abc")
        (with-current-buffer (buf)
          (ok (equalp (vi-option-value "iskeyword") isk)
              "Another buffer's local option is not changed"))))))
