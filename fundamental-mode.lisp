(in-package :lem)

(export '(fundamental-mode))

(define-major-mode fundamental-mode nil
  (:name "fundamental"
   :keymap *global-keymap*))
