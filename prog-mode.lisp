(in-package :lem)

(export '(*prog-mode-keymap*
          prog-mode
          indent-line
          newline-and-indent
          indent-region))

(define-major-mode prog-mode nil
  (:name "prog"
   :keymap *prog-mode-keymap*)
  (setf (get-bvar :indent-tabs-mode) t))

(defun indent-line-internal (column)
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
           (delete-char (length old-indent-string) nil)
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

(defun calc-indent-default ()
  (save-excursion
   (forward-line -1)
   (back-to-indentation)
   (current-column)))

(defun indent-line-1 (default-indent-function)
  (let* ((f (get-bvar :calc-indent-function :default default-indent-function))
         (n (and f (funcall f))))
    (if (and f n)
        (indent-line-internal n)
        (insert-char #\tab 1))))

(define-key *prog-mode-keymap* (kbd "C-i") 'indent-line)
(define-command indent-line () ()
  (indent-line-1 nil))

(define-key *global-keymap* (kbd "C-j") 'newline-and-indent)
(define-key *global-keymap* (kbd "M-j") 'newline-and-indent)
(define-command newline-and-indent (n) ("p")
  (insert-newline n)
  (indent-line-1 #'calc-indent-default))

(define-key *global-keymap* (kbd "C-M-\\") 'indent-region)
(define-command indent-region (begin end) ("r")
  (save-excursion
    (apply-region-lines begin
                        end
                        'indent-line)))

(define-key *prog-mode-keymap* (kbd "C-M-a") 'beginning-of-defun)
(define-command beginning-of-defun (&optional (n 1)) ("p")
  (when (get-bvar :beginning-of-defun-function)
    (funcall (get-bvar :beginning-of-defun-function) n)))

(define-key *prog-mode-keymap* (kbd "C-M-e") 'end-of-defun)
(define-command end-of-defun (&optional (n 1)) ("p")
  (if (get-bvar :end-of-defun-function)
      (funcall (get-bvar :end-of-defun-function) n)
      (beginning-of-defun (- n))))
