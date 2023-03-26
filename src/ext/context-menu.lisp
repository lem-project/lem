(defpackage :lem/context-menu
  (:use :cl :lem)
  (:export :make-item
           :display-context-menu)
  #+sbcl
  (:lock t))
(in-package :lem/context-menu)

(define-attribute description-attribute
  (:dark :foreground "white")
  (:light :foreground "#777"))

(defstruct item
  label
  description
  callback)

(defvar *context-menu-mode-keymap* (make-keymap :name '*context-menu-mode-keymap*
                                                :undef-hook 'context-menu-default))

(define-minor-mode context-menu-mode
    (:name "context-menu"
     :keymap *context-menu-mode-keymap*))

(define-key *context-menu-mode-keymap* 'keyboard-quit 'context-menu-finish)
(define-key *context-menu-mode-keymap* 'escape 'context-menu-finish)
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


(defclass print-spec ()
  ((label-width :initarg :label-width
                :reader print-spec-label-width)))

(defun compute-label-width (items)
  (loop :for item :in items
        :maximize (1+ (length (item-label item)))))

(defmethod lem/popup-window:apply-print-spec ((print-spec print-spec) point item)
  (insert-string point " ")
  (insert-string point (item-label item))
  (move-to-column point (print-spec-label-width print-spec) t)
  (insert-string point " ")
  (when (item-description item)
    (insert-string point (item-description item) :attribute 'description-attribute)
    (insert-string point " ")))

(defun display-context-menu (items)
  (display-popup-menu items
                      :action-callback (lambda (item)
                                         (context-menu-finish)
                                         (funcall (item-callback item)))
                      :print-spec (make-instance 'print-spec
                                                 :label-width (compute-label-width items)))
  (context-menu-mode t))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo" :callback (lambda () (message "select foo")))
                              (make-item :label "bar" :callback (lambda () (message "select bar")))
                              (make-item :label "baz" :callback (lambda () (message "select baz"))))))
