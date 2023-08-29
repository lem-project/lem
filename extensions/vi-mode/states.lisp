(defpackage :lem-vi-mode/states
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :define-vi-state
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
           :*outer-text-objects-keymap*
           :*inner-text-objects-keymap*
           :normal
           :insert
           :operator))
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
(defvar *outer-text-objects-keymap* (make-keymap :name '*outer-text-objects-keymap*))
(defvar *inner-text-objects-keymap* (make-keymap :name '*inner-text-objects-keymap*))

(defvar *inactive-keymap* (make-keymap))

;;
;; Normal state

(define-vi-state normal () ()
  (:default-initargs
   :name "NORMAL"
   :cursor-color "IndianRed"
   :cursor-type :box
   :modeline-color 'state-modeline-yellow
   :keymaps (list *normal-keymap*)))

;;
;; Insert state

(define-vi-state insert () ()
  (:default-initargs
   :name "INSERT"
   :message "-- INSERT --"
   :cursor-color "IndianRed"
   :cursor-type :bar
   :modeline-color 'state-modeline-aqua
   :keymaps (list *insert-keymap*)))

;;
;; Ex state

(define-vi-state vi-modeline () ()
  (:default-initargs
   :name "COMMAND"
   :modeline-color 'state-modeline-green
   :keymaps (list *inactive-keymap*)))

;;
;; Operator-pending state

(define-vi-state operator () ()
  (:default-initargs
   :keymaps (list *operator-keymap* *normal-keymap*)))

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
