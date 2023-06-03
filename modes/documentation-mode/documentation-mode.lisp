(defpackage :lem-documentation-mode
  (:use :cl
        :lem
        :lem-documentation-mode/internal))
(in-package :lem-documentation-mode)

(define-major-mode documentation-mode ()
    (:name "Documentation"
     :keymap *documentation-mode-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *documentation-mode-keymap* "Return" 'documentation-select)

(define-command documentation-select () ()
  (select-link (current-point)))

(define-command documentation-describe-bindings () ()
  (let ((buffer (generate-buffer "*Help*")))
    (change-buffer-mode buffer 'documentation-mode)
    (switch-to-buffer buffer)))
