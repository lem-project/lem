(in-package :lem)

(export '(*lisp-mode-keymap*
          *lisp-syntax-table*
          lisp-mode
          lisp-indent-line
          lisp-newline-and-indent
          eval-string
          eval-region
          eval-defun
          eval-last-sexp
          eval-buffer
          load-file
          go-to-lisp
          macroexpand-lisp
          indent-region-lisp
          indent-sexp
          complete-symbol
          *inferior-lisp-mode-keymap*
          inferior-lisp-mode
          inferior-lisp
          inferior-lisp-eval))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (elt '((block . 1)
                 (case . 1)
                 (ccase . case)
                 (ecase . case)
                 (typecase . case)
                 (etypecase . case)
                 (ctypecase . case)
                 (catch . 1)
                 ;(cond . 0)
                 (defclass . defun)
                 (define-condition . defun)
                 (define-modify-macro . 1)
                 (defsetf . defun)
                 (defun . 2)
                 (define-setf-method . defun)
                 (define-setf-expander . defun)
                 (defmacro . defun)
                 (deftype . defun)
                 (defmethod . defun)
                 (defpackage . 1)
                 (defstruct . 1)
                 (destructuring-bind . 2)
                 (do . 2)
                 (do* . do)
                 (dolist . 1)
                 (dotimes . dolist)
                 (eval-when . 1)
                 (flet . 1)
                 (labels . flet)
                 (macrolet . flet)
                 (generic-flet . flet)
                 (generic-labels . flet)
                 (handler-case . 1)
                 (restart-case . handler-case)
                 (if . 1)
                 (lambda . 1)
                 (let . 1)
                 (let* . let)
                 (handler-bind . let)
                 (restart-bind . let)
                 (locally . 0)
                 (multiple-value-bind . 2)
                 (multiple-value-call . 1)
                 (multiple-value-prog1 . 1)
                 (prog . 1)
                 (prog* . prog)
                 (prog1 . 1)
                 (prog2 . 2)
                 (progn . 0)
                 (progv . 2)
                 (return . 0)
                 (return-from . 1)
                 (symbol-macrolet . let)
                 (tagbody . 0)
                 (throw . 1)
                 (unless . 1)
                 (unwind-protect . 0)
                 (when . 1)
                 (with-accessors . multiple-value-bind)
                 (with-condition-restarts . multiple-value-bind)
                 (with-output-to-string . 1)
                 (with-slots . multiple-value-bind)
                 (with-standard-io-syntax . 2)))
    (setf (get (car elt) 'lisp-indent) (cdr elt))))

(defvar *lisp-mode-keymap*
        (make-keymap "lisp" 'undefined-key *global-keymap*))

(defvar *lisp-syntax-table*
        (make-syntax-table
                 :space-chars '(#\space #\tab #\newline)
                 :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\> #\= #\/ #\:)
                 :paren-alist '((#\( . #\))
                                (#\[ . #\])
                                (#\{ . #\}))
                 :string-quote-chars '(#\")
                 :escape-chars '(#\\)
                 :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
                 :line-comment-char #\;))

(define-major-mode lisp-mode
  :name "lisp-mode"
  :keymap *lisp-mode-keymap*
  :syntax-table *lisp-syntax-table*)

(defun lisp-looking-at-word ()
  (save-excursion
    (skip-chars-forward
     (lambda (c)
       (or (eq c #\space)
           (eq c #\tab))))
    (let ((begin (point)))
      (forward-sexp)
      (list (region-string begin (point))
                   (when (= (window-cur-linum)
                            (progn
                              (skip-chars-forward 'syntax-space-char-p)
                              (window-cur-linum)))
                     (window-cur-col))))))

(defun lisp-count-sexps (goal)
  (do ((count 0 (1+ count)))
      (nil)
    (unless (forward-sexp)
      (return (values count nil)))
    (when (point< goal (point))
      (return (values count t)))))

(define-key *lisp-mode-keymap* (kbd "C-i") 'lisp-indent-line)
(define-command lisp-indent-line () ()
  (beginning-of-line)
  (let ((point (point)))
    (when (and (up-list 1) (down-list 1))
      (let ((start-col (1- (window-cur-col))))
        (destructuring-bind (car-name-str arg-col)
            (lisp-looking-at-word)
          (let* ((car-name (intern (string-upcase car-name-str) :lem))
                 (count (lisp-count-sexps point)))
            (let ((num
                   (do ((val (get car-name 'lisp-indent))
                        (count 0 (1+ count)))
                       ((numberp val) val)
                     (when (< 10 count) (return))
                     (setq val (get val 'lisp-indent)))))
              (point-set point)
              (delete-while-whitespaces t)
              (cond
               ((and (null num)
                     (or (eql 0 (search "define-" car-name-str))
                         (eql 0 (search "with-" car-name-str))
                         (eql 0 (search "do-" car-name-str))))
                (insert-char #\space (+ start-col 2)))
               ((null num)
                (if arg-col
                  (insert-char #\space arg-col)
                  (insert-char #\space (+ start-col 1))))
               ((< (1- count) num)
                (insert-char #\space (+ start-col 4)))
               (t
                (insert-char #\space (+ start-col 2)))))))))))

(define-key *lisp-mode-keymap* (kbd "C-j") 'lisp-newline-and-indent)
(define-command lisp-newline-and-indent (n) ("p")
  (insert-newline n)
  (lisp-indent-line))

(defun string-to-exps (str)
  (let ((str str)
        (exps)
        (eof-value (make-symbol "eof")))
    (do ()
        ((string= "" str))
      (multiple-value-bind (expr i)
          (read-from-string str nil eof-value)
        (when (eq expr eof-value)
          (return))
        (push expr exps)
        (setq str (subseq str i))))
    (cons 'progn (nreverse exps))))

(defun eval-from-string (str)
  (eval (string-to-exps str)))

(defun lisp-error-clause (cdt)
  (let* ((buffer (get-buffer-create "*Error*"))
         (out (make-buffer-output-stream buffer)))
    (popup buffer
           (lambda ()
             (princ cdt out)))
    cdt))

(defvar *eval-thread*)
(defvar *mi-thread*)

(defun safe-eval-from-string-1 (str)
  (unless (get-buffer "*REPL*")
    (let ((*current-window* *current-window*))
      (inferior-lisp)))
  (let ((val))
    (labels ((eval-thread-closure
              ()
              (let ((out
                     (let ((repl-buffer (get-buffer-create "*REPL*")))
                       (make-buffer-output-stream
                        repl-buffer
                        (make-point (buffer-nlines repl-buffer)
                                    0))))
                    (in
                     (make-minibuffer-input-stream)))
                (let ((*error-output* (or out *error-output*))
                      (*standard-output* (or out *standard-output*))
                      (*standard-input* in))
                  (handler-case
                      (prog1 (setq val (eval-from-string str))
                        (point-set (make-point
                                    (buffer-output-stream-linum out)
                                    (buffer-output-stream-column out))))
                    (error (cdt)
                           (setq val cdt)
                           (lisp-error-clause cdt))))))
             (mi-thread-closure
              ()
              (loop for c = (cl-ncurses:getch)
                    do
                    (when (char= key::ctrl-c (code-char c))
                      (bt:destroy-thread *eval-thread*)))))
      (setq *eval-thread* (bt:make-thread #'eval-thread-closure))
      (setq *mi-thread* (bt:make-thread #'mi-thread-closure))
      (bt:join-thread *eval-thread*)
      (bt:destroy-thread *mi-thread*))
    val))

(defun safe-eval-from-string (x)
  (handler-case (safe-eval-from-string-1 x)
    #+sbcl
    (sb-thread:join-thread-error
     (cdt)
     (bt:destroy-thread *mi-thread*)
     (write-message "interrupt")
     nil)))

(defun eval-string (str)
  (write-message
   (write-to-string
    (safe-eval-from-string str))))

(define-command eval-region (&optional begin end) ("r")
  (unless (or begin end)
    (setq begin (region-beginning))
    (setq end (region-end)))
  (eval-string (region-string begin end))
  t)

(define-key *lisp-mode-keymap* (kbd "M-C-x") 'eval-defun)
(define-command eval-defun () ()
  (save-excursion
   (top-of-defun)
   (mark-sexp)
   (eval-region)
   t))

(define-key *lisp-mode-keymap* (kbd "C-xu") 'eval-last-sexp)
(define-command eval-last-sexp () ()
  (save-excursion
   (when (backward-sexp)
     (mark-sexp)
     (eval-region)
     t)))

(define-key *lisp-mode-keymap* (kbd "C-xy") 'eval-buffer)
(define-command eval-buffer () ()
  (save-excursion
   (eval-region (progn (beginning-of-buffer) (point))
                (progn (end-of-buffer) (point)))
   t))

(define-key *lisp-mode-keymap* (kbd "C-xl") 'load-file)
(define-command load-file (filename) ("fLoad File: ")
  (handler-case
      (when (and (file-exist-p filename)
                 (not (file-directory-p filename)))
        (write-message
         (format nil "~a"
                 (load filename))))
    (error (cdt)
           (lisp-error-clause cdt))))

(define-key *lisp-mode-keymap* (kbd "C-xz") 'go-to-lisp)
(define-command go-to-lisp () ()
  (save-some-buffers)
  (dolist (buffer *buffer-list*)
    (when (eq 'lisp-mode (buffer-major-mode buffer))
      (if (buffer-filename buffer)
        (load (buffer-filename buffer))
        (progn
          (set-buffer buffer)
          (eval-buffer)
          (unmark-buffer)))))
  (exit-lem))

(define-key *lisp-mode-keymap* (kbd "C-xm") 'macroexpand-lisp)
(define-command macroexpand-lisp (arg) ("P")
  (let ((expr
         (read-from-string
          (region-string (point)
                         (let ((start (point)))
                           (forward-sexp)
                           (prog1 (point)
                             (point-set start))))
          nil)))
    (let* ((buffer (get-buffer-create "*macroexpand*"))
           (out (make-buffer-output-stream buffer)))
      (popup buffer
             (lambda ()
               (pprint (if arg
                         (macroexpand expr)
                         (macroexpand-1 expr))
                       out))))))

(define-command indent-region-lisp () ()
  (save-excursion
   (apply-region-lines (region-beginning)
                       (region-end)
                       'lisp-indent-line)
   t))

(define-key *lisp-mode-keymap* (kbd "M-C-q") 'indent-sexp)
(define-command indent-sexp () ()
  (mark-sexp)
  (indent-region-lisp))

(define-key *lisp-mode-keymap* (kbd "M-C-i") 'complete-symbol)
(define-command complete-symbol () ()
  (let* ((end (point))
         (begin (prog2 (backward-sexp)
                    (point)
                  (point-set end)))
         (str (region-string begin end)))
    (when (< 0 (length str))
      (let ((upcase-p (char<= #\A (aref str 0) #\Z))
            (symbols))
        (let ((str (string-upcase str)))
          (do-symbols (sym)
            (when (eql 0 (search str (symbol-name sym)))
              (push (if upcase-p 
                      (symbol-name sym)
                      (string-downcase (symbol-name sym)))
                    symbols))))
        (let ((comp-str
               (popup-completion (lambda (str)
                                   (completion str symbols))
                                 str)))
          (insert-string
           (subseq comp-str (length str)))))
      t)))

(defvar *inferior-lisp-log* nil)
(defvar *inferior-lisp-log-back* nil)
(defvar *inferior-lisp-last-point* nil)

(defvar *inferior-lisp-mode-keymap*
        (make-keymap "inferior-lisp" 'undefined-key *lisp-mode-keymap*))

(define-major-mode inferior-lisp-mode
  :name "inferior-lisp-mode"
  :keymap *inferior-lisp-mode-keymap*
  :syntax-table *lisp-syntax-table*)

(define-command inferior-lisp () ()
  (let* ((buffer (get-buffer-create "*REPL*")))
    (setq *current-window* (pop-to-buffer buffer))
    (setq *inferior-lisp-log* nil)
    (setq *inferior-lisp-last-point* nil)
    (inferior-lisp-mode)
    (insert-string "> ")))

(define-key *inferior-lisp-mode-keymap* (kbd "C-m") 'inferior-lisp-eval)
(define-command inferior-lisp-eval () ()
  (let ((point (point)))
    (when (backward-sexp)
      (mark-sexp)
      (let ((str (region-string (region-beginning)
                                (region-end))))
        (push str *inferior-lisp-log*)
        (end-of-buffer)
        (insert-newline 1)
        (insert-string (write-to-string (safe-eval-from-string str)))
        (insert-newline 1)
        (insert-string "> ")
        (setq *inferior-lisp-last-point* (point))))))

(define-key *inferior-lisp-mode-keymap* (kbd "M-p") 'inferior-lisp-prev)
(define-command inferior-lisp-prev () ()
  (when *inferior-lisp-last-point*
    (point-set *inferior-lisp-last-point*)
    (let ((*kill-disable-p* t))
      (kill-sexp)))
  (setq *inferior-lisp-last-point* (point))
  (let ((str (pop *inferior-lisp-log*)))
    (push str *inferior-lisp-log-back*)
    (insert-string str)))

(define-key *inferior-lisp-mode-keymap* (kbd "M-n") 'inferior-lisp-next)
(define-command inferior-lisp-next () ()
  (when *inferior-lisp-last-point*
    (point-set *inferior-lisp-last-point*)
    (let ((*kill-disable-p* t))
      (kill-sexp)))
  (setq *inferior-lisp-last-point* (point))
  (let ((str (pop *inferior-lisp-log-back*)))
    (push str *inferior-lisp-log*)
    (insert-string str)))
