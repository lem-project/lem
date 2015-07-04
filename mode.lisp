(in-package :lem)

(export '(syntax-table
          make-syntax-table
          major-mode
          mode-name
          define-mode
          fundamental-mode
          current-syntax))

(defstruct syntax-table
  (space-chars '(#\space #\tab #\newline))
  symbol-chars
  (paren-alist '((#\( . #\))
                 (#\[ . #\])
                 (#\{ . #\})))
  (string-quote-chars '(#\"))
  (escape-chars '(#\\))
  expr-prefix-chars
  line-comment-char)

(defun major-mode ()
  (or (buffer-major-mode (window-buffer))
    'fundamental-mode))

(defun mode-name ()
  (get (major-mode) 'mode-name))

(defun current-mode-keymap ()
  (get (major-mode) 'keymap))

(defun set-current-mode-keymap (keymap)
  (setf (get (major-mode) 'keymap) keymap))

(defun set-major-mode (major-mode)
  (setf (buffer-major-mode (window-buffer)) major-mode))

(defmacro define-mode (major-mode &key name keymap syntax-table)
  `(progn
    (setf (get ',major-mode 'mode-name) ,name)
    (setf (get ',major-mode 'keymap) ,keymap)
    (setf (get ',major-mode 'syntax-table) ,syntax-table)
    (define-command ,major-mode () ()
      (set-major-mode ',major-mode))))

(define-mode fundamental-mode
  :name "fundamental-mode"
  :keymap *global-keymap*
  :syntax-table (make-syntax-table))

(defun current-syntax ()
  (get (major-mode) 'syntax-table))

(defun syntax-word-char-p (c)
  (alphanumericp c))

(defun syntax-space-char-p (c)
  (member c (syntax-table-space-chars (current-syntax))))

(defun syntax-symbol-char-p (c)
  (or (syntax-word-char-p c)
      (member c (syntax-table-symbol-chars (current-syntax)))))

(defun syntax-open-paren-char-p (c)
  (assoc c (syntax-table-paren-alist (current-syntax))))

(defun syntax-closed-paren-char-p (c)
  (rassoc c (syntax-table-paren-alist (current-syntax))))

(defun syntax-pair-open-paren (c)
  (car (rassoc c (syntax-table-paren-alist (current-syntax)))))

(defun syntax-pair-closed-paren (c)
  (cdr (assoc c (syntax-table-paren-alist (current-syntax)))))

(defun syntax-string-quote-char-p (c)
  (member c (syntax-table-string-quote-chars (current-syntax))))

(defun syntax-escape-char-p (c)
  (member c (syntax-table-escape-chars (current-syntax))))

(defun syntax-expr-prefix-char-p (c)
  (member c (syntax-table-expr-prefix-chars (current-syntax))))

(defun syntax-line-comment-char-p (c)
  (eql c (syntax-table-line-comment-char (current-syntax))))
