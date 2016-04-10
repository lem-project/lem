;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*prog-mode-keymap*
          prog-mode
          prog-indent-line
          prog-newline-and-indent
          prog-indent-region))

(define-major-mode prog-mode nil
  (:name "prog"
   :keymap-var *prog-mode-keymap*)
  (setf (get-bvar :indent-tabs-mode) t))

(defun indent-line (column)
  (when (minusp column) (setq column 0))
  (let* ((old-column (current-column))
         (old-indent-string
           (save-excursion
            (region-string (progn (beginning-of-line) (current-point))
                           (progn (back-to-indentation) (current-point)))))
         (new-indent-string
           (if (get-bvar :indent-tabs-mode :default t)
               (multiple-value-bind (div mod)
                   (floor column *tab-size*)
                 (concatenate 'string
                              (make-string div :initial-element #\tab)
                              (make-string mod :initial-element #\space)))
               (make-string column :initial-element #\space))))
    (cond ((string/= old-indent-string new-indent-string)
           (beginning-of-line)
           (delete-char (length old-indent-string) t)
           (insert-string new-indent-string)
           (if (< old-column column)
               (back-to-indentation)
               (move-to-column
                (max 0
                     (+ old-column
                        (- (str-width new-indent-string)
                           (str-width old-indent-string)))))))
          ((< old-column column)
           (back-to-indentation)))
    t))

(define-key *prog-mode-keymap* (kbd "C-i") 'prog-indent-line)
(define-command prog-indent-line () ()
  (let* ((f (get-bvar :calc-indent-function))
         (n (and f (funcall f))))
    (if n
        (indent-line n)
        (insert-char #\tab 1))))

(define-key *prog-mode-keymap* (kbd "C-j") 'prog-newline-and-indent)
(define-key *prog-mode-keymap* (kbd "M-j") 'prog-newline-and-indent)
(define-command prog-newline-and-indent (n) ("p")
  (insert-newline n)
  (prog-indent-line))

(define-key *prog-mode-keymap* (kbd "C-M-\\") 'prog-indent-region)
(define-command prog-indent-region () ()
  (save-excursion
   (apply-region-lines (region-beginning)
                       (region-end)
                       'prog-indent-line)))
