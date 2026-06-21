(defpackage #:lem-emacs-help-mode
  (:use #:cl #:lem))
(in-package #:lem-emacs-help-mode)

(defvar *previous-ctrl-h-suffix* nil)
(defvar *ctrl-h-keymap* (make-keymap))

(define-key *ctrl-h-keymap* "k" 'describe-key)
(define-key *ctrl-h-keymap* "b" 'describe-bindings)
(define-key *ctrl-h-keymap* "m" 'describe-mode)
(define-key *ctrl-h-keymap* "a" 'apropos-command)

;; TODO add describe-variable command for "v" and
;; add describe-function command for "f"

(defun enable (&optional recursed-p)
  "Enables emacs help mode"
  (let* ((prefixes (keymap-prefixes *global-keymap*))
         (ctrl-h (first (parse-keyspec "C-h")))
         (index (position ctrl-h prefixes :key #'prefix-key :test #'key-equal)))

    (cond
      (index
       (setf *previous-ctrl-h-suffix* (prefix-suffix (nth index prefixes)))
       (setf (prefix-suffix (nth index prefixes)) *ctrl-h-keymap*))
      (recursed-p
       (error "Infinite loop"))
      (t
       (push (make-prefix :key ctrl-h) (keymap-prefixes *global-keymap*))
       (enable t)))))

(defun disable ()
  "Disables emacs help mode"
  (let* ((prefixes (keymap-prefixes *global-keymap*))
         (ctrl-h (first (parse-keyspec "C-h")))
         (index (position ctrl-h prefixes :key #'prefix-key
                          :test #'lem-core:key-equal)))
    (assert index)
    (setf (prefix-suffix (nth index prefixes)) *previous-ctrl-h-suffix*)
    (setf *previous-ctrl-h-suffix* nil)))

(define-minor-mode emacs-help-mode
    (:name "EHelp"
     :description "Adds Emacs Style C-h Bindings."
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))
