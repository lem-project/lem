(defpackage :lem-tests/prompt
  (:use :cl :testif)
  (:import-from :lem-fake-interface
                :fake-interface
                :with-fake-interface))
(in-package :lem-tests/prompt)

(test prompt-for-character
  (with-fake-interface ()
    (lem:unread-key (lem:make-key :sym "a"))
    (ok (equal #\a
               (lem:prompt-for-character
                "test: "
                :gravity (make-instance 'lem.popup-window::gravity-cursor))))))
