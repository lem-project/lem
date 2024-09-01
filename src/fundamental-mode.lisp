(in-package :lem-core)

(define-major-mode lem/buffer/fundamental-mode:fundamental-mode nil
    (:name "Fundamental"
     :require-final-newline nil))

(defvar *global-keymap* (make-keymap :name '*global-keymap*))

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
