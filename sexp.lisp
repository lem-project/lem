(in-package :lem)

(export '(forward-list
          backward-list
          forward-sexp
          backward-sexp
          down-list
          up-list
          beginning-of-defun
          end-of-defun
          mark-sexp
          kill-sexp
          transpose-sexps))

(defun skip-space-forward ()
  (loop for c = (following-char) do
    (cond ((syntax-space-char-p c)
           (unless (next-char 1)
             (return nil)))
          ((and (syntax-line-comment-char-p c)
                (not (sexp-escape-p t)))
           (unless (next-line 1)
             (return nil))
           (beginning-of-line))
          (t
           (return t)))))

(defun line-comment-column ()
  (labels ((f (str i in-string-p)
              (if (>= i (length str))
                  nil
                  (let ((c (schar str i)))
                    (cond ((syntax-escape-char-p c)
                           (f str (+ i 2) in-string-p))
                          (in-string-p
                           (f str (1+ i) t))
                          ((syntax-string-quote-char-p c)
                           (f str (1+ i) (not in-string-p)))
                          ((syntax-line-comment-char-p c)
                           i)
                          (t
                           (f str (1+ i) in-string-p)))))))
    (f (buffer-line-string (window-buffer) (window-cur-linum))
       0
       nil)))

(defun skip-space-backward ()
  (loop
    (if (bolp)
        (if (prev-line 1)
            (let ((col (line-comment-column)))
              (if col
                  (goto-column col)
                  (end-of-line)))
            (return nil))
        (let ((c (preceding-char)))
          (if (or (syntax-space-char-p c)
                  (syntax-line-comment-char-p c))
              (unless (prev-char 1)
                (return nil))
              (return t))))))

(defparameter *converted-char-types*
  '(:space
    :symbol
    :open-paren
    :closed-paren
    :string-quote
    :expr-prefix
    :line-comment))

(defun sexp-get-char (dir)
  (if dir
      (following-char)
      (preceding-char)))

(defun sexp-escape-p (dir)
  (oddp (mod (loop for n from (if dir 1 2)
               while (syntax-escape-char-p
                      (char-before n))
               count 1)
             2)))

(defun sexp-get-syntax-type (dir)
  (if (sexp-escape-p dir)
      :symbol
      (let ((c (sexp-get-char dir)))
        (cond ((syntax-space-char-p c)        :space)
              ((syntax-symbol-char-p c)       :symbol)
              ((syntax-open-paren-char-p c)   :open-paren)
              ((syntax-closed-paren-char-p c) :closed-paren)
              ((syntax-string-quote-char-p c) :string-quote)
              ((syntax-escape-char-p c)       :escape)
              ((syntax-expr-prefix-char-p c)  :expr-prefix)
              ((syntax-line-comment-char-p c) :line-comment)
              (t                              :symbol)))))

(defun sexp-step-char (dir)
  (if dir
      (next-char 1)
      (prev-char 1)))

(defun skip-symbol (dir)
  (loop
    (case (sexp-get-syntax-type dir)
      ((:symbol :expr-prefix :escape)
       (unless (sexp-step-char dir)
         (return)))
      (t
       (return t)))))

(defun skip-symbol-forward ()
  (skip-symbol t))

(defun skip-symbol-backward ()
  (skip-symbol nil))

(defun skip-list (depth dir)
  (loop with paren-char = nil do
    (unless (if dir
                (skip-space-forward)
                (skip-space-backward))
      (return))
    (let ((type (sexp-get-syntax-type dir)))
      (case type
        ((:open-paren :closed-paren)
         (let ((c (sexp-get-char dir))
               (count-flag))
           (cond ((not paren-char)
                  (setq paren-char c)
                  (setq count-flag t))
                 ((syntax-equal-paren-p paren-char c)
                  (setq count-flag t)))
           (when count-flag
             (if (eql type :open-paren)
                 (if dir
                     (incf depth)
                     (decf depth))
                 (if dir
                     (decf depth)
                     (incf depth)))
             (when (and dir
                        (eq type :closed-paren)
                        (minusp depth))
               (return nil))
             (when (and (not dir)
                        (eq type :open-paren)
                        (minusp depth))
               (return nil))
             (sexp-step-char dir)
             (when (zerop depth)
               (return t)))))
        ((:string-quote)
         (skip-string dir))
        (otherwise
         (sexp-step-char dir))))))

(defun skip-list-forward (depth)
  (skip-list depth t))

(defun skip-list-backward (depth)
  (skip-list depth nil))

(defun skip-string (dir)
  (loop with quote-char = (sexp-get-char dir) do
    (unless (sexp-step-char dir)
      (return))
    (when (and (eq :string-quote (sexp-get-syntax-type dir))
               (char= quote-char (sexp-get-char dir)))
      (sexp-step-char dir)
      (return t))))

(defun skip-string-forward ()
  (skip-string t))

(defun skip-string-backward ()
  (skip-string nil))

(defun skip-sexp-forward ()
  (loop
    (case (sexp-get-syntax-type t)
      ((:symbol :escape)
       (return (skip-symbol-forward)))
      ((:expr-prefix)
       (unless (next-char 1)
         (return)))
      ((:open-paren)
       (return (skip-list-forward 0)))
      ((:closed-paren)
       (return))
      ((:string-quote)
       (return (skip-string-forward)))
      (otherwise
       (return t)))))

(defun skip-sexp-backward ()
  (ecase (sexp-get-syntax-type nil)
    ((:symbol :expr-prefix :escape)
     (skip-symbol-backward))
    ((:closed-paren)
     (skip-list-backward 0)
     (loop while (eq :expr-prefix (sexp-get-syntax-type nil))
       do (unless (prev-char 1)
            (return))
       finally (return t)))
    ((:open-paren)
     nil)
    ((:string-quote)
     (skip-string-backward))))

(define-key *global-keymap* (kbd "M-C-f") 'forward-sexp)
(define-command forward-sexp (&optional (n 1)) ("p")
  (let ((skip-space
         (if (plusp n)
             #'skip-space-forward
             #'skip-space-backward))
        (skip-sexp
         (if (plusp n)
             #'skip-sexp-forward
             #'skip-sexp-backward)))
    (dotimes (_ (abs n) t)
      (unless (and (funcall skip-space)
                   (funcall skip-sexp))
        (return nil)))))

(define-key *global-keymap* (kbd "M-C-b") 'backward-sexp)
(define-command backward-sexp (&optional (n 1)) ("p")
  (forward-sexp (- n)))

(defun scan-lists (n depth)
  (let ((dir (plusp n))
        (point (point)))
    (dotimes (_ (abs n) t)
      (unless (skip-list depth dir)
        (point-set point)
        (return)))))

(define-key *global-keymap* (kbd "M-C-n") 'forward-list)
(define-command forward-list (&optional (n 1)) ("p")
  (scan-lists n 0))

(define-key *global-keymap* (kbd "M-C-p") 'backward-list)
(define-command backward-list (&optional (n 1)) ("p")
  (scan-lists (- n) 0))

(define-key *global-keymap* (kbd "M-C-d") 'down-list)
(define-command down-list (&optional (n 1)) ("p")
  (scan-lists n -1))

(define-key *global-keymap* (kbd "M-C-u") 'up-list)
(define-command up-list (&optional (n 1)) ("p")
  (scan-lists (- n) 1))

(defun top-of-defun ()
  (loop while (up-list 1) count 1))

(define-key *global-keymap* (kbd "M-C-a") 'beginning-of-defun)
(define-command beginning-of-defun (&optional (n 1)) ("p")
  (if (minusp n)
      (end-of-defun (- n))
      (dotimes (_ n t)
        (when (zerop (top-of-defun))
          (unless (backward-sexp 1)
            (return nil))))))

(define-key *global-keymap* (kbd "M-C-e") 'end-of-defun)
(define-command end-of-defun (&optional (n 1)) ("p")
  (if (minusp n)
      (beginning-of-defun (- n))
      (dotimes (_ n t)
        (top-of-defun)
        (unless (forward-sexp 1)
          (return nil))
        (loop
          for c = (following-char)
          do (cond ((char= c #\newline)
                    (next-line 1)
                    (beginning-of-line)
                    (return t))
                   ((syntax-space-char-p c)
                    (next-char 1))
                   (t
                    (return t)))))))

(define-key *global-keymap* (kbd "M-C-@") 'mark-sexp)
(define-command mark-sexp () ()
  (save-excursion
   (and (forward-sexp 1)
        (mark-set))))

(define-key *global-keymap* (kbd "M-C-k") 'kill-sexp)
(define-command kill-sexp (&optional (n 1)) ("p")
  (and (mark-sexp)
       (kill-region (region-beginning)
                    (region-end))))

(define-key *global-keymap* (kbd "M-C-t") 'transpose-sexps)
(define-command transpose-sexps () ()
  (let ((point (point)))
    (cond ((not (and (forward-sexp 1)
                     (backward-sexp 2)))
           (point-set point)
           nil)
          (t
           (kill-sexp 1)
           (setq *kill-new-flag* t)
           (delete-while-whitespaces nil t)
           (setq *kill-new-flag* t)
           (kill-sexp 1)
           (yank 1)
           (yank 2)
           (yank 3)
           (setq *kill-new-flag* t)
           t))))
