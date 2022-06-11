(in-package :lem-lisp-mode)

(defun display-message (control-string &rest format-arguments)
  (show-message (apply #'format nil control-string format-arguments)
                :style '(:gravity :cursor
                         :use-border nil
                         :background-color "#404040"
                         :offset-x 1
                         :offset-y 1)
                :timeout nil))
