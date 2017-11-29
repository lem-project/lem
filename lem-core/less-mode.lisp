(defpackage :lem.less-mode
  (:use :cl :lem)
  (:export :less-mode))
(in-package :lem.less-mode)

(define-minor-mode less-mode
    (:name "less"
     :keymap *less-mode-keymap*)
  (setf *read-only-function* 'less-mode)
  (setf (buffer-read-only-p (current-buffer))
        (mode-active-p (current-buffer) 'less-mode)))

(define-key *less-mode-keymap* "e" 'less-forward-line)
;; (define-key *less-mode-keymap* "C-e" 'less-forward-line)
(define-key *less-mode-keymap* "j" 'less-forward-line)
;; (define-key *less-mode-keymap* "C-n" 'less-forward-line)
(define-key *less-mode-keymap* "C-m" 'less-forward-line)
(define-key *less-mode-keymap* "C-j" 'less-forward-line)

(define-key *less-mode-keymap* "y" 'less-backward-line)
(define-key *less-mode-keymap* "C-y" 'less-backward-line)
(define-key *less-mode-keymap* "k" 'less-backward-line)
(define-key *less-mode-keymap* "C-k" 'less-backward-line)
;; (define-key *less-mode-keymap* "C-p" 'less-backward-line)

(define-key *less-mode-keymap* "f" 'less-forward-window)
;; (define-key *less-mode-keymap* "C-f" 'less-forward-window)
(define-key *less-mode-keymap* "C-v" 'less-forward-window)
(define-key *less-mode-keymap* "Spc" 'less-forward-window)
(define-key *less-mode-keymap* "z" 'less-forward-window)
(define-key *less-mode-keymap* "M-Spc" 'less-forward-window)

(define-key *less-mode-keymap* "b" 'less-backward-window)
;; (define-key *less-mode-keymap* "C-b" 'less-backward-window)
(define-key *less-mode-keymap* "M-v" 'less-backward-window)
(define-key *less-mode-keymap* "w" 'less-backward-window)

(define-key *less-mode-keymap* "d" 'less-forward-half-window)
(define-key *less-mode-keymap* "C-d" 'less-forward-half-window)

(define-key *less-mode-keymap* "u" 'less-backward-half-window)
(define-key *less-mode-keymap* "C-u" 'less-backward-half-window)

(define-key *less-mode-keymap* "r" 'recenter)
(define-key *less-mode-keymap* "C-r" 'recenter)
(define-key *less-mode-keymap* "C-l" 'recenter)

(define-key *less-mode-keymap* "/" 'less-search-forward)
(define-key *less-mode-keymap* "?" 'less-search-backward)

(define-key *less-mode-keymap* "n" 'less-repeat-search-forward)
(define-key *less-mode-keymap* "N" 'less-repeat-search-backward)
(define-key *less-mode-keymap* "M-u" 'less-toggle-search-highlighting)

(define-key *less-mode-keymap* "g" 'less-go-to-first-line)
(define-key *less-mode-keymap* "<" 'less-go-to-first-line)
(define-key *less-mode-keymap* "M-<" 'less-go-to-first-line)

(define-key *less-mode-keymap* "G" 'less-go-to-last-line)
(define-key *less-mode-keymap* ">" 'less-go-to-last-line)
(define-key *less-mode-keymap* "M->" 'less-go-to-last-line)

(define-key *less-mode-keymap* "p" 'less-go-to-n-percent)
(define-key *less-mode-keymap* "%" 'less-go-to-n-percent)

(defvar *arg* nil)

(defun arg () (or *arg* 1))

(define-command less-forward-line () ()
  (scroll-down (arg)))

(define-command less-backward-line () ()
  (scroll-up (arg)))

(define-command less-forward-window () ()
  (dotimes (_ (arg))
    (next-page)))

(define-command less-backward-window () ()
  (dotimes (_ (arg))
    (previous-page)))

(define-command less-forward-half-window () ()
  (scroll-down (floor (window-height) 2)))

(define-command less-backward-half-window () ()
  (scroll-up (floor (window-height) 2)))

(define-command less-search-forward () ()
  (lem.isearch:isearch-forward-regexp))

(define-command less-search-backward () ()
  (lem.isearch:isearch-backward-regexp))

(define-command less-repeat-search-forward () ()
  (lem.isearch:isearch-next))

(define-command less-repeat-search-backward () ()
  (lem.isearch:isearch-prev))

(define-command less-toggle-search-highlighting () ()
  (lem.isearch:isearch-toggle-highlighting))

(define-command less-go-to-first-line () ()
  (move-to-beginning-of-buffer))

(define-command less-go-to-last-line () ()
  (move-to-end-of-buffer))

(define-command less-go-to-n-percent () ()
  (when (<= 0 (arg) 100)
    (move-to-beginning-of-buffer)
    (scroll-down (floor (* (arg) (window-height)) 100))))
