(defpackage :lem-select-popup
  (:use :cl :lem)
  (:export :start-select-popup))

(in-package :lem-select-popup)

(defvar *select-popup-mode-keymap* (make-keymap :name '*select-popup-mode-keymap*
                                          :undef-hook 'nop))

(define-minor-mode select-popup-mode
    (:name "select-popup"
     :keymap *select-popup-mode-keymap*))

(defun select-popup-end ()
  (select-popup-mode nil)
  (lem-if:popup-menu-quit (implementation)))

(define-command nop () ()
  (unless (insertion-key-p (last-read-key-sequence))
    (unread-key-sequence (last-read-key-sequence))
    (select-popup-end)))
  

(define-command select-popup-next-line () ()
  (lem-if:popup-menu-down (implementation)))

(define-command select-popup-previous-line () ()
  (lem-if:popup-menu-up (implementation)))

(define-command select-popup-select () ()
  (lem-if:popup-menu-select (implementation)))

(define-key *select-popup-mode-keymap* 'next-line 'select-popup-next-line)
(define-key *select-popup-mode-keymap* "M-n" 'select-popup-next-line)
(define-key *select-popup-mode-keymap* 'previous-line 'select-popup-previous-line)
(define-key *select-popup-mode-keymap* "Return" 'select-popup-select)

(defun start-select-popup (alist)
  (lem-if:display-popup-menu (implementation)
                             alist
                             :action-callback (lambda (item)
                                                (and (cdr item)
                                                     (funcall (cdr item) item)
                                                (select-popup-end)))
                             :print-spec 'car)
  (select-popup-mode t))
