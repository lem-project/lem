(in-package :lem-core)

(define-major-mode lem/buffer/fundamental-mode:fundamental-mode nil
    (:name "Fundamental"))

(defvar *global-keymap* (make-keymap :description '*global-keymap*))

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
