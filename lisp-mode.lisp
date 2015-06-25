(in-package :lem)

(defvar *special-indent-symbols*
        (dolist (elt '((block 1)
                       (let* 1)
                       (catch 1)
                       (eval-when 1)
                       (symbol-macrolet 1)
                       (flet 1)
                       (macrolet 1)
                       (tagbody 0)
                       (multiple-value-prog1 1)
                       (if 1)
                       (progn 0)
                       (unwind-protect 1)
                       (labels 1)
                       (progv 1)
                       (let 1)
                       (defun 2)
                       (defmacro 2)
                       (lambda 1)
                       (when 1)
                       (unless 1)
                       (dotimes 1)
                       (dolist 1)
                       ))
          (setf (get (car elt) 'lisp-indent) (cadr elt))))

(defvar *lisp-mode-keymap*
  (make-keymap "lisp" 'undefined-key *global-keymap*))

(define-mode lisp-mode
  :name "lisp-mode"
  :keymap *lisp-mode-keymap*
  :syntax-table (make-syntax-table
                 :space-chars '(#\space #\tab #\newline)
                 :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\>)
                 :paren-alist '((#\( . #\))
                                (#\[ . #\])
                                (#\{ . #\}))
                 :string-quote-chars '(#\")
                 :escape-chars '(#\\)
                 :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
                 :comment-starter-chars '(#\;)
                 :comment-ender-chars '(#\newline)))

(define-key *lisp-mode-keymap* "C-i" 'lisp-indent-line)
(defcommand lisp-indent-line () ()
  (beginning-of-line)
  (delete-while-whitespaces t)
  (let (col
        num
        cadr-col
        (start-point (point)))
    (do ()
        ((not (backward-sexp))
         (setq col (window-cur-col))
         (setq num
               (let ((point (point))
                     (first-linum (window-cur-linum)))
                 (prog1
                  (do ((count 0 (1+ count)))
                      ((point< start-point (point))
                       (1- count))
                      (unless (forward-sexp)
                        (return count))
                      (skip-chars-forward 'syntax-space-char-p)
                      (when (and (= count 0)
                                 (= first-linum (window-cur-linum)))
                        (setq cadr-col (window-cur-col))))
                  (point-set point))))))
    (let ((chars))
      (skip-chars-forward
       (lambda (c)
         (when (syntax-symbol-char-p c)
           (push c chars)
           t)))
      (point-set start-point)
      (let* ((first (intern (string-upcase (coerce (nreverse chars) 'string))
                            :lem))
             (indent (get first 'lisp-indent)))
        (if indent
          (if (< num indent)
            (insert-char #\space (+ col 3))
            (insert-char #\space (+ col 1)))
          (if cadr-col
            (insert-char #\space cadr-col)
            (insert-char #\space col)))))))

(define-key *lisp-mode-keymap* "M-j" 'newline-and-indent)
(defcommand newline-and-indent (n) ("p")
  (insert-newline n)
  (lisp-indent-line))
