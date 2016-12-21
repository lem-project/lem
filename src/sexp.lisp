(in-package :lem)

(export '(raw-forward-sexp
          forward-sexp
          backward-sexp
          scan-lists
          forward-list
          backward-list
          down-list
          up-list
          top-of-defun
          mark-sexp
          kill-sexp
          transpose-sexps
          beginning-of-defun-abstract))

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

(defun sexp-escape-p (dir &optional (offset 0))
  (oddp (mod (loop for n from (- (if dir 1 2) offset)
               while (syntax-escape-char-p
                      (char-before n))
               count 1)
             2)))

(defun sexp-get-syntax-type (dir)
  (if (sexp-escape-p dir)
      :symbol
      (let ((c (sexp-get-char dir)))
        (cond ((syntax-space-char-p c) :space)
              ((syntax-symbol-char-p c) :symbol)
              ((syntax-open-paren-char-p c) :open-paren)
              ((syntax-closed-paren-char-p c) :closed-paren)
              ((syntax-string-quote-char-p c) :string-quote)
              ((syntax-escape-char-p c) :escape)
              ((syntax-expr-prefix-char-p c) :expr-prefix)
              ((syntax-line-comment-p c
                                      (if dir
                                          (char-after 1)
                                          (following-char)))
               :line-comment)
              (t :symbol)))))

(defun sexp-step-char (dir)
  (shift-position (if dir 1 -1)))

(defun skip-symbol (dir)
  (loop
    (if (and (null dir) (bobp))
        (return t)
        (case (sexp-get-syntax-type dir)
          ((:symbol :expr-prefix :escape)
           (unless (sexp-step-char dir)
             (return)))
          (t
           (return t))))))

(defun skip-symbol-forward ()
  (skip-symbol t))

(defun skip-symbol-backward ()
  (skip-symbol nil))

(defun skip-list (depth dir)
  (let ((paren-stack))
    (loop
      (unless (if dir
                  (skip-space-and-comment-forward)
                  (skip-space-and-comment-backward))
        (return nil))
      (when (if dir (eobp) (bobp))
        (return nil))
      (let ((type (sexp-get-syntax-type dir)))
        (case type
          ((:open-paren :closed-paren)
           (let ((c (sexp-get-char dir)))
             (cond ((or (and (eq type :open-paren) dir)
                        (and (eq type :closed-paren) (not dir)))
                    (push c paren-stack)
                    (incf depth))
                   ((or (and (not (>= 0 depth)) (null paren-stack))
                        (syntax-equal-paren-p c (car paren-stack)))
                    (pop paren-stack)
                    (decf depth))
                   (t
                    (return nil)))
             (sexp-step-char dir)
             (when (zerop depth)
               (return t))))
          ((:string-quote)
           (skip-string dir))
          ((:symbol)
           (skip-symbol dir))
          (otherwise
           (sexp-step-char dir)))))))

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
  (syntax-skip-expr-prefix-forward)
  (loop
    (when (eobp)
      (return))
    (case (sexp-get-syntax-type t)
      ((:symbol :escape)
       (return (skip-symbol-forward)))
      ((:expr-prefix)
       (unless (shift-position 1)
         (return)))
      ((:open-paren)
       (return (skip-list-forward 0)))
      ((:closed-paren)
       (return))
      ((:string-quote)
       (return (skip-string-forward)))
      ((:line-comment)
       (end-of-line)
       (shift-position 1))
      (otherwise
       (return t)))))

(defun skip-sexp-backward ()
  (if (bobp)
      nil
      (prog1 (ecase (sexp-get-syntax-type nil)
               ((:symbol :expr-prefix :escape)
                (skip-symbol-backward))
               ((:closed-paren)
                (skip-list-backward 0))
               ((:open-paren)
                nil)
               ((:string-quote)
                (skip-string-backward))
               ((:line-comment)
                (loop :while (eq :line-comment (sexp-get-syntax-type nil)) :do
                  (shift-position -1))
                t))
        (loop while (eq :expr-prefix (sexp-get-syntax-type nil))
          do (unless (shift-position -1)
               (return))
          finally (return t))
        (syntax-skip-expr-prefix-backward))))

(defun raw-forward-sexp (n)
  (let ((skip-space
         (if (plusp n)
             #'skip-space-and-comment-forward
             #'skip-space-and-comment-backward))
        (skip-sexp
         (if (plusp n)
             #'skip-sexp-forward
             #'skip-sexp-backward)))
    (let ((point (current-point)))
      (dotimes (_ (abs n) t)
        (funcall skip-space)
        (unless (funcall skip-sexp)
          (point-set point)
          (return nil))))))

(define-key *global-keymap* (kbd "C-M-f") 'forward-sexp)
(define-command forward-sexp (&optional (n 1) no-errors) ("p")
  (let* ((f (get-bvar :forward-sexp-function))
         (res (if f
                  (funcall f n)
                  (raw-forward-sexp n))))
    (or res
        (if no-errors
            nil
            (editor-error "scan error")))))

(define-key *global-keymap* (kbd "C-M-b") 'backward-sexp)
(define-command backward-sexp (&optional (n 1) no-errors) ("p")
  (forward-sexp (- n) no-errors))

(defun scan-lists (n depth &optional no-errors)
  (let ((dir (plusp n))
        (point (current-point)))
    (dotimes (_ (abs n) t)
      (unless (skip-list depth dir)
        (point-set point)
        (unless no-errors
          (editor-error "scan error"))
        (return)))))

(define-key *global-keymap* (kbd "C-M-n") 'forward-list)
(define-command forward-list (&optional (n 1) no-errors) ("p")
  (scan-lists n 0 no-errors))

(define-key *global-keymap* (kbd "C-M-p") 'backward-list)
(define-command backward-list (&optional (n 1) no-errors) ("p")
  (scan-lists (- n) 0 no-errors))

(define-key *global-keymap* (kbd "C-M-d") 'down-list)
(define-command down-list (&optional (n 1) no-errors) ("p")
  (scan-lists n -1 no-errors))

(define-key *global-keymap* (kbd "C-M-u") 'up-list)
(define-command up-list (&optional (n 1) no-errors) ("p")
  (scan-lists (- n) 1 no-errors))

(defun top-of-defun ()
  (loop while (up-list 1 t))
  t)

(define-key *global-keymap* (kbd "C-M-@") 'mark-sexp)
(define-command mark-sexp () ()
  (save-excursion
   (and (forward-sexp 1)
        (mark-set))))

(define-key *global-keymap* (kbd "C-M-k") 'kill-sexp)
(define-command kill-sexp (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (let ((end (form-offset (copy-marker (current-marker) :temporary) 1)))
      (if end
          (kill-region (current-marker) end)
          (editor-error "scan error")))))

(define-key *global-keymap* (kbd "C-M-t") 'transpose-sexps)
(define-command transpose-sexps () ()
  (backward-sexp 1)
  (kill-sexp 1)
  (kill-ring-new)
  (delete-while-whitespaces nil t)
  (kill-ring-new)
  (kill-sexp 1)
  (yank 1)
  (yank 2)
  (yank 3)
  (kill-ring-new)
  t)

(defun beginning-of-defun-abstract (n match-p)
  (let ((arg (if (plusp n) 1 -1)))
    (dotimes (_ (abs n) t)
      (when (bolp)
        (forward-line (- arg)))
      (loop
        (beginning-of-line)
        (when (funcall match-p)
          (return t))
        (unless (forward-line (- arg))
          (return nil))))))
