(in-package :lem)

(export '(forward-sexp
          backward-sexp
          scan-lists
          forward-list
          backward-list
          down-list
          up-list
          mark-sexp
          kill-sexp
          transpose-sexps))

(defun sexp-scan-error ()
  (editor-error "scan error"))

(defun %sexp-escape-p (point offset)
  (let ((count 0))
    (loop :with string := (line-string-at point)
          :for i :downfrom (+ (1- (marker-charpos point)) offset) :to 0
          :do (if (syntax-escape-char-p (schar string i))
                  (incf count)
                  (return)))
    (oddp count)))

(defun %sexp-symbol-p (c)
  (or (syntax-symbol-char-p c)
      (syntax-escape-char-p c)
      (syntax-expr-prefix-char-p c)))

(defun %skip-symbol-forward (point)
  (skip-chars-forward point #'%sexp-symbol-p)
  point)

(defun %skip-symbol-backward (point)
  (skip-chars-backward point #'%sexp-symbol-p)
  point)

(defun %skip-string-forward (point)
  (loop :with quote-char := (character-at point 0) :do
        (unless (nth-value 1 (character-offset point 1))
          (return nil))
        (let ((c (character-at point)))
          (cond ((syntax-escape-char-p c)
                 (character-offset point 1))
                ((and (syntax-string-quote-char-p c)
                      (char= c quote-char))
                 (character-offset point 1)
                 (return point))))))

(defun %skip-string-backward (point)
  (character-offset point -1)
  (loop :with quote-char := (character-at point) :do
        (unless (nth-value 1 (character-offset point -1))
          (return nil))
        (if (%sexp-escape-p point 0)
            (character-offset point -1)
            (let ((c (character-at point)))
              (cond ((and (syntax-string-quote-char-p c)
                          (char= c quote-char))
                     (return point)))))))

(defun %skip-list-forward (point depth)
  (loop :with paren-stack := '() :do
        (unless (skip-space-and-comment-forward point)
          (return nil))
        (when (end-buffer-p point)
          (return nil))
        (let ((c (character-at point 0)))
          (cond ((syntax-open-paren-char-p c)
                 (push c paren-stack)
                 (character-offset point 1)
                 (when (zerop (incf depth))
                   (return point)))
                ((syntax-closed-paren-char-p c)
                 (unless (or (and (< 0 depth)
                                  (null paren-stack))
                             (syntax-equal-paren-p c (car paren-stack)))
                   (return nil))
                 (pop paren-stack)
                 (character-offset point 1)
                 (when (zerop (decf depth))
                   (return point)))
                ((syntax-string-quote-char-p c)
                 (%skip-string-forward point))
                ((syntax-escape-char-p c)
                 (character-offset point 2))
                (t
                 (character-offset point 1))))))

(defun %skip-list-backward (point depth)
  (loop :with paren-stack := '() :do
        (unless (skip-space-and-comment-backward point)
          (return nil))
        (when (start-buffer-p point)
          (return nil))
        (let ((c (character-at point -1)))
          (cond ((%sexp-escape-p point -1)
                 (character-offset point -1))
                ((syntax-closed-paren-char-p c)
                 (push c paren-stack)
                 (character-offset point -1)
                 (when (zerop (incf depth))
                   (return point)))
                ((syntax-open-paren-char-p c)
                 (unless (or (and (< 0 depth)
                                  (null paren-stack))
                             (syntax-equal-paren-p c (car paren-stack)))
                   (return nil))
                 (pop paren-stack)
                 (character-offset point -1)
                 (when (zerop (decf depth))
                   (return point)))
                ((syntax-string-quote-char-p c)
                 (%skip-string-backward point))
                (t
                 (character-offset point -1))))))

(defun %form-offset-positive (point)
  (skip-space-and-comment-forward point)
  (syntax-skip-expr-prefix-forward point)
  (skip-chars-forward point #'syntax-expr-prefix-char-p)
  (unless (end-buffer-p point)
    (let ((c (character-at point)))
      (cond ((or (syntax-symbol-char-p c)
                 (syntax-escape-char-p c))
             (%skip-symbol-forward point))
            ((syntax-expr-prefix-char-p c)
             (multiple-value-bind (point moved)
                 (character-offset point 1)
               (when moved
                 point)))
            ((syntax-open-paren-char-p c)
             (%skip-list-forward point 0))
            ((syntax-closed-paren-char-p c)
             nil)
            ((syntax-string-quote-char-p c)
             (%skip-string-forward point))))))

(defun %form-offset-negative (point)
  (skip-space-and-comment-backward point)
  (let ((c (character-at point -1)))
    (prog1 (cond ((or (syntax-symbol-char-p c)
                      (syntax-escape-char-p c)
                      (syntax-expr-prefix-char-p c))
                  (%skip-symbol-backward point))
                 ((syntax-closed-paren-char-p c)
                  (%skip-list-backward point 0))
                 ((syntax-open-paren-char-p c)
                  nil)
                 ((syntax-string-quote-char-p c)
                  (%skip-string-backward point)))
      (skip-chars-backward point #'syntax-expr-prefix-char-p)
      (syntax-skip-expr-prefix-backward point))))

(defun form-offset (point n)
  (with-marker ((prev point))
    (cond ((plusp n)
           (dotimes (_ n point)
             (unless (%form-offset-positive point)
               (move-point point prev)
               (return nil))))
          (t
           (dotimes (_ (- n) point)
             (unless (%form-offset-negative point)
               (move-point point prev)
               (return nil)))))))

(defun scan-lists (point n depth &optional no-errors)
  (with-marker ((prev point))
    (cond ((plusp n)
           (dotimes (_ n point)
             (unless (%skip-list-forward point depth)
               (move-point point prev)
               (if no-errors
                   (return nil)
                   (sexp-scan-error)))))
          (t
           (dotimes (_ (- n) point)
             (unless (%skip-list-backward point depth)
               (move-point point prev)
               (if no-errors
                   (return nil)
                   (sexp-scan-error))))))))

(define-key *global-keymap* (kbd "C-M-f") 'forward-sexp)
(define-command forward-sexp (&optional (n 1) no-errors) ("p")
  (with-marker ((prev (current-marker)))
    (let ((point (form-offset (current-marker) n)))
      (or point
          (progn
            (move-point (current-marker) prev)
            (if no-errors
                nil
                (sexp-scan-error)))))))

(define-key *global-keymap* (kbd "C-M-b") 'backward-sexp)
(define-command backward-sexp (&optional (n 1) no-errors) ("p")
  (forward-sexp (- n) no-errors))

(define-key *global-keymap* (kbd "C-M-n") 'forward-list)
(define-command forward-list (&optional (n 1) no-errors) ("p")
  (scan-lists (current-marker) n 0 no-errors))

(define-key *global-keymap* (kbd "C-M-p") 'backward-list)
(define-command backward-list (&optional (n 1) no-errors) ("p")
  (scan-lists (current-marker) (- n) 0 no-errors))

(define-key *global-keymap* (kbd "C-M-d") 'down-list)
(define-command down-list (&optional (n 1) no-errors) ("p")
  (scan-lists (current-marker) n -1 no-errors))

(define-key *global-keymap* (kbd "C-M-u") 'up-list)
(define-command up-list (&optional (n 1) no-errors) ("p")
  (scan-lists (current-marker) (- n) 1 no-errors))

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
          (sexp-scan-error)))))

(define-key *global-keymap* (kbd "C-M-t") 'transpose-sexps)
(define-command transpose-sexps () ()
  (with-marker ((point1 (current-marker) :left-inserting)
                (point2 (current-marker) :left-inserting))
    (let ((form-string1
           (let ((start (form-offset point1 -1))
                 (end (form-offset (copy-marker point1 :temporary) 1)))
             (prog1 (points-to-string start end)
               (delete-between-points start end))))
          (form-string2
           (let ((end (form-offset point2 1))
                 (start (form-offset (copy-marker point2 :temporary) -1)))
             (prog1 (points-to-string start end)
               (delete-between-points start end)))))
      (insert-string-at point1 form-string2)
      (insert-string-at point2 form-string1))))
