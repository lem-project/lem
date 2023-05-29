(in-package :lem-base)

(lem-core:define-major-mode fundamental-mode nil
    (:name "Fundamental"))

(in-package :lem-core)

(defvar *global-keymap* (make-keymap :name '*global-keymap*))

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
