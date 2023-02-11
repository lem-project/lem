(in-package :lem-lisp-mode)

(defun display-message (control-string &rest format-arguments)
  (show-message (apply #'format nil control-string format-arguments)
                :style '(:gravity :cursor
                         :use-border t
                         :background-color "#404040")
                :timeout nil))
