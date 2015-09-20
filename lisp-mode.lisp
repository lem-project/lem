(in-package :lem)

(export '(*lisp-mode-keymap*
          *lisp-syntax-table*
          lisp-mode
          lisp-indent-line
          lisp-newline-and-indent
          lisp-eval-string
          lisp-eval-region
          lisp-eval-defun
          lisp-eval-last-sexp
          lisp-eval-buffer
          lisp-load-file
          lisp-macroexpand
          lisp-describe-symbol
          lisp-indent-region
          lisp-indent-sexp
          lisp-complete-symbol
          lisp-comment-region
          popup-scratch-buffer
          *scratch-mode-keymap*
          scratch-mode
          eval-print-last-sexp
          lisp-info-popup))

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
                 ;(if . 1)
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
                 (with-standard-io-syntax . 2)
                 (defparameter . 1)
                 (defvar . 1)
                 (defconstant . 1)
                 (loop . 0)))
    (setf (get (car elt) 'lisp-indent) (cdr elt))))

(defvar *lisp-mode-keymap*
  (make-keymap "lisp"))

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
   :expr-prefix-forward-function 'lisp-mode-skip-expr-prefix-forward
   :expr-prefix-backward-function 'lisp-mode-skip-expr-prefix-backward
   :line-comment-preceding-char #\;
   :block-comment-preceding-char #\#
   :block-comment-following-char #\|))

(syntax-add-keyword *lisp-syntax-table*
                    "("
                    :regex-p nil
                    :matched-symbol :start-expr
                    :symbol-tov 1)

(syntax-add-keyword *lisp-syntax-table*
                    "[^() \\t]+"
                    :regex-p t
                    :test-symbol :define-start
                    :attr :function-name-attr)

(dolist (str '("defun"
               "defclass"
               "defsetf"
               "defmacro"
               "deftype"
               "defmethod"
               "defpackage"
               "defstruct"
               "defparameter"
               "defconstant"))
  (syntax-add-keyword *lisp-syntax-table*
                      str
                      :regex-p nil
                      :word-p t
                      :test-symbol :start-expr
                      :attr :keyword-attr
                      :matched-symbol :define-start
                      :symbol-tov 1))

(syntax-add-keyword *lisp-syntax-table*
                    "^(:?[^: \\t]+:)?define-[^ \\t()]*$"
                    :regex-p t
                    :word-p t
                    :test-symbol :start-expr
                    :attr :keyword-attr
                    :matched-symbol :define-start
                    :symbol-tov 1)

(dolist (str '("block"
               "case"
               "ccase"
               "defvar"
               "ecase"
               "typecase"
               "etypecase"
               "ctypecase"
               "catch"
               "cond"
               "destructuring-bind"
               "do"
               "do*"
               "dolist"
               "dotimes"
               "eval-when"
               "flet"
               "labels"
               "macrolet"
               "generic-flet"
               "generic-labels"
               "handler-case"
               "restart-case"
               "if"
               "lambda"
               "let"
               "let*"
               "handler-bind"
               "restart-bind"
               "locally"
               "multiple-value-bind"
               "multiple-value-call"
               "multiple-value-prog1"
               "prog"
               "prog*"
               "prog1"
               "prog2"
               "progn"
               "progv"
               "return"
               "return-from"
               "symbol-macrolet"
               "tagbody"
               "throw"
               "unless"
               "unwind-protect"
               "when"
               "with-accessors"
               "with-condition-restarts"
               "with-output-to-string"
               "with-slots"
               "with-standard-io-syntax"
               "loop"))
  (syntax-add-keyword *lisp-syntax-table*
                      str
                      :regex-p nil
                      :word-p t
                      :test-symbol :start-expr
                      :attr :keyword-attr))

(syntax-add-keyword *lisp-syntax-table*
                    "^\\*[^*]+\\*$"
                    :regex-p t
                    :word-p t
                    :attr :variable-attr)

(syntax-add-keyword *lisp-syntax-table*
                    "^:[^() \\t]+$"
                    :regex-p t
                    :word-p t
                    :attr :constant-attr)

(syntax-add-keyword *lisp-syntax-table*
                    "^&[^() \\t]+$"
                    :regex-p t
                    :word-p t
                    :attr :constant-attr)

(define-major-mode lisp-mode nil
  (:name "lisp"
   :keymap *lisp-mode-keymap*
   :syntax-table *lisp-syntax-table*))

(defun %lisp-mode-skip-expr-prefix (c1 c2 step-arg)
  (when c1
    (multiple-value-bind (_ dispatch-char-p)
        (get-macro-character c1)
      (when (and dispatch-char-p
                 (not (eql c2 #\())
                 (get-dispatch-macro-character c1 c2))
        (next-char step-arg)))))

(defun lisp-mode-skip-expr-prefix-forward ()
  (%lisp-mode-skip-expr-prefix (char-after 0)
                               (char-after 1)
                               2))

(defun lisp-mode-skip-expr-prefix-backward ()
  (%lisp-mode-skip-expr-prefix (char-before 2)
                               (char-before 1)
                               -2))

(defun lisp-looking-at-word ()
  (save-excursion
   (skip-chars-forward
    #'(lambda (c)
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

(defun %count-sexps (goal)
  (do ((count 0 (1+ count)))
      ((or (not (forward-sexp 1 t))
           (eobp)
           (point< goal (point)))
       count)))

(define-key *lisp-mode-keymap* (kbd "C-i") 'lisp-indent-line)
(define-command lisp-indent-line () ()
  (beginning-of-line)
  (let ((point (point)))
    (when (and (up-list 1 t) (down-list 1 t))
      (let ((start-col (1- (window-cur-col))))
        (destructuring-bind (car-name-str arg-col)
            (lisp-looking-at-word)
          (let* ((car-symbol-name
                  (string-upcase
                   (car
                    (last (split-string car-name-str
                                        #\:)))))
                 (car-name (intern car-symbol-name :lem))
                 (argc (%count-sexps point)))
            (let ((num
                   (do ((val (get car-name 'lisp-indent)))
                       ((or (not val)
                            (numberp val))
                        val)
                     (setq val (get val 'lisp-indent)))))
              (point-set point)
              (delete-while-whitespaces t)
              (cond
               ((or (char= #\( (aref car-name-str 0))
                    (char= #\: (aref car-name-str 0))
                    (char= #\" (aref car-name-str 0)))
                (insert-char #\space (+ start-col 1)))
               ((and (null num)
                     (or (eql 0 (search "DEFINE-" car-symbol-name))
                         (eql 0 (search "WITH-" car-symbol-name))
                         (eql 0 (search "DO-" car-symbol-name))))
                (insert-char #\space (+ start-col 2)))
               ((null num)
                (if arg-col
                    (insert-char #\space arg-col)
                    (insert-char #\space (+ start-col 1))))
               ((< (1- argc) num)
                (insert-char #\space (+ start-col 4)))
               (t
                (insert-char #\space (+ start-col 2)))))))))))

(define-key *lisp-mode-keymap* (kbd "C-j") 'lisp-newline-and-indent)
(define-key *lisp-mode-keymap* (kbd "M-j") 'lisp-newline-and-indent)
(define-command lisp-newline-and-indent (n) ("p")
  (insert-newline n)
  (lisp-indent-line))

(define-command lisp-indent-region () ()
  (save-excursion
   (apply-region-lines (region-beginning)
                       (region-end)
                       'lisp-indent-line)
   t))

(define-key *lisp-mode-keymap* (kbd "M-C-q") 'lisp-indent-sexp)
(define-command lisp-indent-sexp () ()
  (mark-sexp)
  (lisp-indent-region))

(defun %string-to-exps (str)
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

(defvar *lisp-eval-thread*)
(defvar *lisp-mi-thread*)
(defvar *lisp-eval-thread-values*)
(defvar *lisp-eval-thread-error-p*)

(defun %make-eval-closure (str output-buffer point update-point-p)
  #'(lambda ()
      (let* ((out (make-buffer-output-stream output-buffer point t))
             (*error-output* out)
             (*trace-output* out)
             (*debug-io* out)
             (*standard-output* out)
             (*standard-input* (make-minibuffer-input-stream))
             (*getch-wait-flag* t))
        (handler-case
            (handler-bind ((error #'lisp-debugger))
              (unwind-protect
                (progn
                  (setq *lisp-eval-thread-values*
                        (multiple-value-list (eval (%string-to-exps str))))
                  (when update-point-p
                    (point-set
                     (buffer-output-stream-point out))))
                (getch-flush)))
          (error (cdt)
                 (setq *lisp-eval-thread-error-p* t)
                 (setq *lisp-eval-thread-values* (list cdt)))))))

(defun %lisp-mi-thread ()
  (loop :for char := (code-char (cl-charms/low-level:getch))
    :if (char= C-g char)
    :do (bt:destroy-thread *lisp-eval-thread*)
    :else
    :do (ungetch char)))

(defun eval-string (str output-buffer point &optional update-point-p)
  (setq *lisp-eval-thread-values* nil)
  (setq *lisp-eval-thread-error-p* nil)
  (setq *lisp-eval-thread*
        (bt:make-thread
         (%make-eval-closure str
                             output-buffer
                             point
                             update-point-p)))
  (setq *lisp-mi-thread* (bt:make-thread #'%lisp-mi-thread))
  (handler-case (bt:join-thread *lisp-eval-thread*)
    #+sbcl
    (sb-thread:join-thread-error (cdt)
                                 (declare (ignore cdt))
                                 (minibuf-print "interrupt")))
  (bt:destroy-thread *lisp-mi-thread*)
  (values *lisp-eval-thread-values*
          *lisp-eval-thread-error-p*))

(defun lisp-eval-string (str)
  (let ((output-buffer (get-buffer-create "*OUTPUT*")))
    (setf (buffer-modified-p output-buffer) nil)
    (prog1 (minibuf-print
            (write-to-string
             (first (eval-string str output-buffer (point-min)))))
      (when (buffer-modified-p output-buffer)
        (lisp-info-popup output-buffer)))))

(define-command lisp-eval-region (&optional begin end) ("r")
  (unless (or begin end)
    (setq begin (region-beginning))
    (setq end (region-end)))
  (lisp-eval-string (region-string begin end))
  t)

(defun %eval-sexp (move-sexp)
  (let ((str (save-excursion
              (and (funcall move-sexp)
                   (mark-sexp)
                   (region-string (region-beginning) (region-end))))))
    (when str
      (lisp-eval-string str)
      t)))

(define-key *lisp-mode-keymap* (kbd "M-C-x") 'lisp-eval-defun)
(define-command lisp-eval-defun () ()
  (%eval-sexp #'top-of-defun))

(define-key *lisp-mode-keymap* (kbd "C-x u") 'lisp-eval-last-sexp)
(define-command lisp-eval-last-sexp () ()
  (%eval-sexp #'backward-sexp))

(define-key *lisp-mode-keymap* (kbd "C-x y") 'lisp-eval-buffer)
(define-command lisp-eval-buffer () ()
  (lisp-eval-string
   (region-string (progn (beginning-of-buffer) (point))
                  (progn (end-of-buffer) (point)))))

(define-key *lisp-mode-keymap* (kbd "C-x l") 'lisp-load-file)
(define-key *lisp-mode-keymap* (kbd "C-x C-l") 'lisp-load-file)
(define-command lisp-load-file (filename) ("fLoad File: ")
  (when (and (file-exist-p filename)
             (not (file-directory-p filename)))
    (lisp-eval-string
     (format nil "(load ~s)" filename))))

(defmacro with-safe-form (&body body)
  `(handler-case
       (handler-bind ((error #'lisp-print-error))
         (values (progn ,@body) nil))
     (error (cdt) (values cdt t))))

(define-key *lisp-mode-keymap* (kbd "C-x m") 'lisp-macroexpand)
(define-command lisp-macroexpand (arg) ("P")
  (multiple-value-bind (expr error-p)
      (with-safe-form
        (let ((expr
               (read-from-string
                (region-string (point)
                               (let ((start (point)))
                                 (forward-sexp)
                                 (prog1 (point)
                                   (point-set start))))
                nil)))
          (setq expr
                (if arg
                    (macroexpand expr)
                    (macroexpand-1 expr)))))
    (unless error-p
      (lisp-info-popup (get-buffer-create "*macroexpand*")
                       #'(lambda (out)
                           (pprint expr out))))))

(define-key *lisp-mode-keymap* (kbd "C-x d") 'lisp-describe-symbol)
(define-command lisp-describe-symbol (name) ("sDescribe: ")
  (multiple-value-bind (str error-p)
      (with-safe-form
        (with-output-to-string (out)
          (describe (read-from-string name) out)))
    (unless error-p
      (lisp-info-popup (get-buffer-create "*describe*")
                       #'(lambda (out)
                           (princ str out))))))

(define-key *lisp-mode-keymap* (kbd "M-C-i") 'lisp-complete-symbol)
(define-command lisp-complete-symbol () ()
  (let* ((end (point))
         (begin (prog2 (backward-sexp)
                    (point)
                  (point-set end)))
         (str (region-string begin end))
         (pkg :cl-user))
    (setq str
          (string-left-trim '(#\' #\` #\,)
                            (string-left-trim '(#\#)
                                              str)))
    (let ((list (split-string str #\:)))
      (when (cdr list)
        (setq pkg (intern (string-upcase (car list))))
        (cond ((string= pkg "")
               (setq pkg :keyword))
              ((not (find-package pkg))
               (setq pkg :cl-user))))
      (setq str (car (last list))))
    (when (< 0 (length str))
      (let ((upcase-p (char<= #\A (aref str 0) #\Z))
            (symbols))
        (let ((str (string-upcase str)))
          (do-symbols (sym pkg)
            (when (eql 0 (search str (symbol-name sym)))
              (push (if upcase-p 
                        (symbol-name sym)
                        (string-downcase (symbol-name sym)))
                    symbols))))
        (let ((comp-str
               (popup-completion #'(lambda (str)
                                     (completion str symbols))
                                 str)))
          (insert-string
           (subseq comp-str (length str)))))
      t)))

(define-key *global-keymap* (kbd "C-x ;") 'lisp-comment-region)
(define-command lisp-comment-region (arg) ("P")
  (let ((begin (region-beginning))
        (end (region-end)))
    (apply-region-lines
     begin
     end
     (if arg
         #'(lambda ()
             (beginning-of-line)
             (when (equal #\; (following-char))
               (delete-char)))
         #'(lambda ()
             (beginning-of-line)
             (insert-string ";"))))))

(define-key *lisp-mode-keymap* (kbd "C-x z") 'popup-scratch-buffer)
(define-key *lisp-mode-keymap* (kbd "C-x C-z") 'popup-scratch-buffer)
(define-command popup-scratch-buffer () ()
  (setq *current-window*
        (pop-to-buffer (get-buffer-create "*scratch*")))
  t)

(defvar *scratch-mode-keymap*
  (make-keymap "scratch" nil *lisp-mode-keymap*))

(define-major-mode scratch-mode nil
  (:name "scratch"
   :keymap *scratch-mode-keymap*
   :syntax-table *lisp-syntax-table*))

(define-key *scratch-mode-keymap* (kbd "C-j") 'eval-print-last-sexp)
(define-command eval-print-last-sexp () ()
  (let ((point (point)))
    (when (backward-sexp)
      (let ((str (string-trim '(#\newline #\tab #\space)
                              (region-string (point) point))))
        (point-set point)
        (unless (string= str "")
          (insert-newline 1)
          (multiple-value-bind (values error-p)
              (eval-string str (current-buffer) (point) t)
            (unless error-p
              (dolist (v values)
                (insert-string (write-to-string v))
                (insert-newline 1)))))))))

(defun lisp-print-error (condition)
  (lisp-info-popup (get-buffer-create "*ERROR*")
                   #'(lambda (out)
                       (format out "~a~%~%" condition)
                       #+sbcl (sb-debug:backtrace 100 out))))

(defun lisp-debugger (condition)
  (let* ((choices (compute-restarts condition))
         (n (length choices)))
    (lisp-info-popup (get-buffer-create "*ERROR*")
                     #'(lambda (out)
                         (format out "~a~%~%" condition)
                         (loop
                           for choice in choices
                           for i from 1
                           do (format out "~&[~d] ~a~%" i choice))
                         (terpri out)
                         #+sbcl (sb-debug:backtrace 100 out)))
    (window-update-all)
    (let ((i (minibuf-read-number "Continue: " 1 n)))
      (invoke-restart-interactively (nth i choices))))
  condition)

(defun lisp-info-popup (buffer &optional fn)
  (funcall (info-popup-closure 'lisp-mode)
           buffer fn t))

(setq *auto-mode-alist*
      (append '((".lisp$" . lisp-mode)
                (".asd$" . lisp-mode))
              *auto-mode-alist*))
