(in-package :lem-core)

(defun maybe-quickload (systems &rest keys &key error-on-failure-p &allow-other-keys)
  (cond
    ((uiop:featurep :quicklisp)
     (apply #'uiop:symbol-call :quicklisp :quickload systems keys))
    (t (if error-on-failure-p
           (apply #'asdf:load-systems systems)
           (ignore-errors (apply #'asdf:load-systems systems))))))
