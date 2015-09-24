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
          ;;lisp-eval-buffer
          ;;lisp-load-file
          ;;lisp-macroexpand
          ;;lisp-macroexpand-all
          ;;lisp-describe-symbol
          ;;lisp-disassemble-symbol
          ;;lisp-indent-region
          ;;lisp-indent-sexp
          ;;lisp-complete-symbol
          ;;lisp-comment-or-uncomment-region
          ;;lisp-comment-region
          ;;lisp-uncomment-region
          ;;*lisp-repl-mode-keymap*
          ;;lisp-repl-mode
          ;;run-lisp
          ;;lisp-repl-return
          ;;lisp-repl-prev-input
          ;;lisp-repl-next-input
          ;;popup-scratch-buffer
          ;;*scratch-mode-keymap*
          ;;scratch-mode
          ;;eval-print-last-sexp
          ;;lisp-info-popup
          ))

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
    (multiple-value-bind (unused-fn dispatch-char-p)
        (get-macro-character c1)
      (declare (ignore unused-fn))
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
               ((and (< 0 (length car-name-str))
                     (or (char= #\( (aref car-name-str 0))
                         (char= #\: (aref car-name-str 0))
                         (char= #\" (aref car-name-str 0))))
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

(defvar *lisp-hostname* "localhost")
(defvar *lisp-port* 12345)
(defvar *lisp-package* "COMMON-LISP-USER")

(define-command lisp-connection (hostname port) ("sHostname:" "nPort:")
  (setq *lisp-hostname* hostname)
  (setq *lisp-port* port)
  t)

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
    (if (= 1 (length exps))
        (car exps)
        (cons 'progn (nreverse exps)))))

(defun eval-from-string (string)
  (swank-client:with-slime-connection (c *lisp-hostname* *lisp-port*)
    (swank-client:slime-eval `(write-to-string
                               (progn
                                 (in-package ,*lisp-package*)
                                 (eval (read-from-string ,string nil))))
                             c)))

(define-command lisp-eval-string (string) ("sEval: ")
  (minibuf-print (eval-from-string string)))

(define-command lisp-eval-region (&optional
                                  (begin (region-beginning))
                                  (end (region-end))) ("r")
  (lisp-eval-string (region-string begin end)))

(defun %eval-sexp (move-sexp)
  (save-excursion
   (and (funcall move-sexp)
        (mark-sexp))
   (lisp-eval-region)))

(define-key *lisp-mode-keymap* (kbd "M-C-x") 'lisp-eval-defun)
(define-command lisp-eval-defun () ()
  (%eval-sexp #'top-of-defun))

(define-key *lisp-mode-keymap* (kbd "C-x u") 'lisp-eval-last-sexp)
(define-command lisp-eval-last-sexp () ()
  (%eval-sexp #'backward-sexp))

(setq *auto-mode-alist*
      (append '((".lisp$" . lisp-mode)
                (".asd$" . lisp-mode))
              *auto-mode-alist*))
