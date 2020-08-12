(defpackage auto-fill-mode
  (:use #:cl #:lem))
(in-package :auto-fill-mode)

(defparameter *max-length* 80)

(define-minor-mode auto-fill-mode
    (:keymap *auto-fill-mode-keymap*
     :name "Auto-Fill"))

(define-key *auto-fill-mode-keymap* "Space"
  'insert-space-and-maybe-newline)
(define-key *auto-fill-mode-keymap* "C-x f"
  'auto-fill-mode-max-columns-current)
(define-key *auto-fill-mode-keymap* "C-x g"
  'auto-fill-mode-max-columns-prompt)

(defun not-syntax-space-char-p (char)
  (not (syntax-space-char-p char)))

(define-command insert-space-and-maybe-newline (&optional n) ("p")
  (declare (ignore n))
  (symbol-macrolet ((point (current-point)))
    (cond
      ((< (point-column point) *max-length*)
       (insert-character point #\Space))
      (t
       (move-to-column point *max-length*)
       (skip-chars-backward point #'not-syntax-space-char-p)
       (let ((whitespaces (skip-whitespace-backward point t)))
         (delete-character point whitespaces))
       (insert-character point #\NewLine)
       (move-to-end-of-line)))))

(defun set-max-length (max-length)
  (message "Setting maximal number of columns to ~d (was ~d)"
           max-length *max-length*)
  (setq *max-length* max-length))

(define-command auto-fill-mode-max-columns-current (n) ("p")
  (declare (ignore n))
  (set-max-length (point-column (current-point))))

(define-command auto-fill-mode-max-columns-prompt (max-length)
    ((list (prompt-for-integer "Enter maximal number of columns: ")))
  (set-max-length max-length))
