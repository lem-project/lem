(defpackage :lem-vi-mode/states
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :define-state
                :*enable-hook*
                :*disable-hook*
                :change-state
                :state-enabled-hook)
  (:import-from :lem-vi-mode/modeline
                :state-modeline-yellow
                :state-modeline-aqua
                :state-modeline-green
                :change-element-by-state)
  (:export :*normal-keymap*
           :*motion-keymap*
           :*insert-keymap*
           :*inactive-keymap*
           :*operator-keymap*
           :*replace-state-keymap*
           :*outer-text-objects-keymap*
           :*inner-text-objects-keymap*
           :normal
           :insert
           :operator
           :replace-state))
(in-package :lem-vi-mode/states)

(defmethod state-enabled-hook :after (state)
  (change-element-by-state state))

;;
;; Keymaps

(defvar *emacs-keymap* *global-keymap*)

(defvar *motion-keymap* (make-keymap :name '*motion-keymap*))
(defvar *normal-keymap* (make-keymap :name '*normal-keymap*
                                     :parent *motion-keymap*))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap*))
(defvar *operator-keymap* (make-keymap :name '*operator-keymap*))
(defvar *replace-state-keymap* (make-keymap :name '*replace-state-keymap*
                                            :undef-hook 'return-last-read-char))
(defvar *outer-text-objects-keymap* (make-keymap :name '*outer-text-objects-keymap*))
(defvar *inner-text-objects-keymap* (make-keymap :name '*inner-text-objects-keymap*))

(defvar *inactive-keymap* (make-keymap))

(define-command return-last-read-char () ()
  (key-to-char (first (lem-core:last-read-key-sequence))))

;;
;; Normal state

(define-state normal () ()
  (:default-initargs
   :name "NORMAL"
   :cursor-type :box
   :modeline-color 'state-modeline-yellow
   :keymaps (list *normal-keymap*)))

;;
;; Insert state

(define-state insert () ()
  (:default-initargs
   :name "INSERT"
   :cursor-color "IndianRed"
   :cursor-type :bar
   :modeline-color 'state-modeline-aqua
   :keymaps (list *insert-keymap*)))

;;
;; Ex state

(define-state vi-modeline () ()
  (:default-initargs
   :name "COMMAND"
   :modeline-color 'state-modeline-green
   :keymaps (list *inactive-keymap*)))

;;
;; Operator-pending state

(define-state operator (normal) ()
  (:default-initargs
   :keymaps (list *operator-keymap* *normal-keymap*)))

;;
;; Replace state

(define-state replace-state (normal) ()
  (:default-initargs
   :cursor-type :underline
   :keymaps (list *replace-state-keymap*)))

;;
;; Setup hooks

(defun enable-normal-state ()
  (change-state 'normal))
(defun enable-vi-modeline-state ()
  (change-state 'vi-modeline))

(defun vi-enable-hook ()
  (change-state 'normal)
  (add-hook *prompt-activate-hook* 'enable-vi-modeline-state)
  (add-hook *prompt-deactivate-hook* 'enable-normal-state))

(defun vi-disable-hook ()
  (remove-hook *prompt-activate-hook* 'enable-vi-modeline-state)
  (remove-hook *prompt-deactivate-hook* 'enable-normal-state))

(add-hook *enable-hook* 'vi-enable-hook)
(add-hook *disable-hook* 'vi-disable-hook)
