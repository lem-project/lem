(in-package :lem)

(export '(fundamental-mode
          emacs-mode))

(define-major-mode fundamental-mode nil
    (:name "fundamental"
     :keymap *global-keymap*))

(define-global-mode emacs-mode ()
  (:keymap *global-keymap*))
