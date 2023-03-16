(defpackage :lem-lsp-mode/context-menu
  (:use :cl :lem)
  (:export :make-item
           :display-context-menu))
(in-package :lem-lsp-mode/context-menu)

(defstruct item
  label
  callback)

(defvar *context-menu-mode-keymap* (make-keymap :name '*context-menu-mode-keymap*
                                                :undef-hook 'lsp-context-menu-default))

(define-minor-mode lsp-context-menu-mode
    (:name "context-menu"
     :keymap *context-menu-mode-keymap*))

(define-key *context-menu-mode-keymap* 'next-line 'lsp-context-menu-next-line)
(define-key *context-menu-mode-keymap* 'previous-line 'lsp-context-menu-previous-line)
(define-key *context-menu-mode-keymap* "Return" 'lsp-context-menu-select)

(defun context-menu-finish ()
  (lsp-context-menu-mode nil)
  (popup-menu-quit))

(define-command lsp-context-menu-default () ()
  (context-menu-finish))

(define-command lsp-context-menu-next-line () ()
  (popup-menu-down))

(define-command lsp-context-menu-previous-line () ()
  (popup-menu-up))

(define-command lsp-context-menu-select () ()
  (popup-menu-select))

(defun display-context-menu (items)
  (display-popup-menu items
                      :action-callback (lambda (item)
                                         (context-menu-finish)
                                         (funcall (item-callback item)))
                      :print-spec #'item-label)
  (lsp-context-menu-mode t))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo" :callback (lambda () (message "select foo")))
                              (make-item :label "bar" :callback (lambda () (message "select bar")))
                              (make-item :label "baz" :callback (lambda () (message "select baz"))))))
