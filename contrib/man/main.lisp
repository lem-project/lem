(uiop/package:define-package :lem-man/main (:use :cl :lem))
(in-package :lem-man/main)
;;;don't edit above

(define-command man (str) ("sManual entry: ")
  (let ((result (sn.man:man str)))
    (if result
        (with-pop-up-typeout-window (out (make-buffer (format nil "*Man ~A" str)) :focus t :erase t)
          (format out "~A" result))
        (message "no entry ~A" str))))
