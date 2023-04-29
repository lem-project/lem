(in-package :lem-base)

(lem:define-major-mode fundamental-mode nil
    (:name "Fundamental"))

(in-package :lem)

(defvar *global-keymap* (make-keymap :name '*global-keymap*
                                     :undef-hook 'self-insert))

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
