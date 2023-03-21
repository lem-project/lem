(defpackage :lem/context-menu
  (:use :cl :lem)
  (:export :make-item
           :display-context-menu)
  #+sbcl
  (:lock t))
(in-package :lem/context-menu)

(defstruct item
  label
  callback)

(defvar *context-menu-mode-keymap* (make-keymap :name '*context-menu-mode-keymap*
                                                :undef-hook 'context-menu-default))

(define-minor-mode context-menu-mode
    (:name "context-menu"
     :keymap *context-menu-mode-keymap*))

(define-key *context-menu-mode-keymap* 'keyboard-quit 'context-menu-finish)
(define-key *context-menu-mode-keymap* 'lem::escape 'context-menu-finish)
(define-key *context-menu-mode-keymap* 'next-line 'context-menu-next-line)
(define-key *context-menu-mode-keymap* 'previous-line 'context-menu-previous-line)
(define-key *context-menu-mode-keymap* "Return" 'context-menu-select)

(define-command context-menu-finish () ()
  (context-menu-mode nil)
  (popup-menu-quit))

(define-command context-menu-default () ()
  )

(define-command context-menu-next-line () ()
  (popup-menu-down))

(define-command context-menu-previous-line () ()
  (popup-menu-up))

(define-command context-menu-select () ()
  (popup-menu-select))

(defun display-context-menu (items)
  (display-popup-menu items
                      :action-callback (lambda (item)
                                         (context-menu-finish)
                                         (funcall (item-callback item)))
                      :print-spec #'item-label)
  (context-menu-mode t))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo" :callback (lambda () (message "select foo")))
                              (make-item :label "bar" :callback (lambda () (message "select bar")))
                              (make-item :label "baz" :callback (lambda () (message "select baz"))))))
