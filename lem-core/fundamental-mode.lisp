(in-package :lem)

(define-major-mode fundamental-mode nil
    (:name "fundamental"))

(define-global-mode emacs-mode ()
  (:keymap *global-keymap*))
