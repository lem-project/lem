(in-package :lem-language-server)

(defun eval-previous-form (point)
  (let ((string (previous-form-string point)))
    (remote-eval-sync *server*
                      `(micros:interactive-eval ,string)
                      (scan-current-package point))))
