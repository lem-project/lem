;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(*lserver-hostname*
          *lserver-port*
          *lserver-package*
          lserver-connection
          lserver-eval-string
          lserver-eval-region
          lserver-eval-defun
          lserver-eval-last-sexp))

(defvar *lserver-hostname* "localhost")
(defvar *lserver-port* 4005)
(defvar *lserver-package* "COMMON-LISP-USER")

(defvar *lserver-mode-keymap* (make-keymap "lserver"))

(define-minor-mode lserver-mode
  :name "lserver"
  :keymap *lserver-mode-keymap*)

(define-command lserver-start () ()
  (lisp-mode)
  (lserver-mode))

(define-command lserver-connection (hostname port) ("sHostname:" "nPort:")
  (setq *lserver-hostname* hostname)
  (setq *lserver-port* port)
  t)

(defun lserver-eval-string-internal (string)
  (swank-client:with-slime-connection (c *lserver-hostname* *lserver-port*)
    (swank-client:slime-eval `(write-to-string
                               (progn
                                 (in-package ,*lserver-package*)
                                 (eval (read-from-string ,string nil))))
                             c)))

(define-command lserver-eval-string (string) ("sEval: ")
  (minibuf-print (lserver-eval-string-internal string)))

(define-command lserver-eval-region (&optional
                                     (begin (region-beginning))
                                     (end (region-end))) ("r")
  (lserver-eval-string (region-string begin end)))

(defun %lserver-eval-sexp (move-sexp)
  (save-excursion
   (and (funcall move-sexp)
        (mark-sexp))
   (lserver-eval-region)))

(define-key *lserver-mode-keymap* (kbd "M-C-x") 'lserver-eval-defun)
(define-command lserver-eval-defun () ()
  (%lserver-eval-sexp #'top-of-defun))

(define-key *lserver-mode-keymap* (kbd "C-x u") 'lserver-eval-last-sexp)
(define-command lserver-eval-last-sexp () ()
  (%lserver-eval-sexp #'backward-sexp))
