(defpackage :lem.fb
  (:use :cl :lem :lem.menu-mode)
  (:export 
   ))
(in-package :lem.fb)


(defparameter *fbar-path* "~/")

(define-key *global-keymap* "C-x ?" 'list-buffers)

(define-major-mode fb-mode menu-mode
    (:name "fb"
     :keymap *fb-menu-mode-keymap*))

(define-command list-files () ()
  (let ((menu (make-instance 'menu
                             :buffer-name "*fb*"
                             :columns '(" " "Name"))))
    (dolist (pth (uiop:subdirectories *fbar-path*))
      (let ((item (make-instance
                   'menu-item
                   :select-function (let ((item-name (enough-namestring pth)))
                                      (lambda (select-function)
					(format xcb::*q* "SELET_FUNCTION ~A~&" select-function)))
                   :name (enough-namestring pth))))
        (append-menu-item item " ")
        (append-menu-item item (enough-namestring pth))
        (append-menu menu item)))
    (display-menu menu 'fb-mode)))
