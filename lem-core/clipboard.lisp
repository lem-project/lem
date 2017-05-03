(defpackage :lem.clipboard
  (:use :cl)
  (:export :copy
           :paste))
(in-package :lem.clipboard)

(defun copy (string)
  (with-input-from-string (input string)
    (ignore-errors
     (uiop:run-program "xclip -i -selection clipboard"
                       :input input
                       :ignore-error-status t)))
  (values))

(defun paste ()
  (with-output-to-string (output)
    (ignore-errors
     (uiop:run-program "xclip -o -selection clipboard"
                       :output output
                       :ignore-error-status t))))
