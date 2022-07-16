(in-package :lem-base)

(lem:define-major-mode fundamental-mode nil
    (:name "fundamental"))

(in-package :lem)

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
