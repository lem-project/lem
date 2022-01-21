(defpackage :lem-tests/self-insert-command
  (:use :cl
        :lem
        :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/self-insert-command)

(defun verify-self-insert (expected-text key-seq)
  (erase-buffer (current-buffer))
  (execute-key-sequence key-seq)
  (ok (equal expected-text (buffer-text (current-buffer)))))

(deftest self-insert-command
  (with-fake-interface ()
    (verify-self-insert "a" (list (make-key :sym "a")))
    (verify-self-insert "aaaa" (list (make-key :ctrl t :sym "u")
                                     (make-key :sym "a")))
    (handler-case
        (progn
          (execute-key-sequence (list (make-key :super t :meta t :hypher t :sym "a")))
          (fail "unreachable"))
      (editor-error (e)
        (ok (search "Key not found: " (princ-to-string e)))))))
