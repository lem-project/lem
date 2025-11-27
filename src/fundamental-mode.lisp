(in-package :lem-core)

(define-major-mode lem/buffer/fundamental-mode:fundamental-mode nil
    (:name "Fundamental"))

(defvar *global-keymap* (make-keymap :name '*global-keymap*))
(defvar *global-prefix-keymap* (make-keymap :name '*global-prefix-keymap*) 
  "The global prefix keymap. It defaults to \"C-x\". ")

(define-key *global-keymap* "C-x" *global-prefix-keymap*)

(define-global-mode emacs-mode ()
  (:name "emacs"
   :keymap *global-keymap*))
