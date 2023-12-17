(defpackage :lem-core/commands/font
  (:use :cl :lem-core)
  (:export :font-size-increase
           :font-size-decrease))
(in-package :lem-core/commands/font)

(define-key *global-keymap* "C-+" 'font-size-increase)
(define-key *global-keymap* "C--" 'font-size-decrease)

(define-command font-size-increase () ()
  "Make the font larger (this currently only works with SDL2 frontend)"
  (lem-if:increase-font-size (implementation)))

(define-command font-size-decrease () ()
  "Make the font smaller (this currently only works with SDL2 frontend)"
  (lem-if:decrease-font-size (implementation)))

(define-command set-font-size (size) ("nSize: ")
  (display:with-display (display)
    (display:with-renderer (display)
      (let ((font-config (display:display-font-config display)))
        (display:change-font 
         display 
         (font:change-size font-config size))))))