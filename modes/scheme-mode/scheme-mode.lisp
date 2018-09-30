(in-package :lem-scheme-mode)

(define-major-mode scheme-mode language-mode
    (:name "scheme"
     :keymap *scheme-mode-keymap*
     :syntax-table lem-scheme-syntax:*syntax-table*
     :mode-hook *scheme-mode-hook*)
  (setf (variable-value 'beginning-of-defun-function) 'scheme-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'scheme-end-of-defun)
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'scheme-calc-indent)
  (setf (variable-value 'line-comment) ";")
  (setf (variable-value 'insertion-line-comment) ";; ")
  (setf (variable-value 'completion-spec) 'scheme-completion)
  (set-syntax-parser lem-scheme-syntax:*syntax-table* (make-tmlanguage-scheme)))

(define-key *scheme-mode-keymap* "C-M-q" 'scheme-indent-sexp)
(define-key *scheme-mode-keymap* "C-c C-e" 'scheme-eval-last-expression)
(define-key *scheme-mode-keymap* "C-c C-r" 'scheme-eval-region)

(defvar *scheme-run-command* "gosh")
(defvar *scheme-run-options* '("-i"))

(defvar *scheme-completion-names*
  (lem-scheme-syntax:get-scheme-completion-data))

(defun scheme-calc-indent (point)
  (lem-scheme-syntax:calc-indent point))

(defun scheme-beginning-of-defun (point n)
  (lem-scheme-syntax:beginning-of-defun point (- n)))

(defun scheme-end-of-defun (point n)
  (if (minusp n)
      (scheme-beginning-of-defun point (- n))
      (dotimes (_ n)
        (with-point ((p point))
          (cond ((and (lem-scheme-syntax:beginning-of-defun p -1)
                      (point<= p point)
                      (or (form-offset p 1)
                          (progn
                            (move-point point p)
                            (return)))
                      (point< point p))
                 (move-point point p)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1)))
                (t
                 (form-offset point 1)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1))))))))

(define-command scheme-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-region (current-point) end))))

(defun scheme-completion (point)
  (with-point ((start point)
               (end point))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (when (point< start end)
      (mapcar (lambda (name)
                (make-completion-item :label name
                                      :start start
                                      :end end))
              (completion (points-to-string start end)
                          *scheme-completion-names*
                          :test #'alexandria:starts-with-subseq)))))

(define-command scheme-scratch () ()
  (let ((buffer (make-buffer "*tmp*")))
    (change-buffer-mode buffer 'scheme-mode)
    (switch-to-buffer buffer)))

(pushnew (cons "\\.scm$" 'scheme-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.sld$" 'scheme-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "\\.rkt$" 'scheme-mode) *auto-mode-alist* :test #'equal)
