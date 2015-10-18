;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(*lisp-mode-keymap*
          *lisp-syntax-table*
          lisp-mode
          lisp-indent
          lisp-indent-line
          lisp-newline-and-indent
          lisp-read-change-package
          lisp-eval-string
          lisp-eval-region
          lisp-eval-defun
          lisp-eval-last-sexp
          lisp-eval-buffer
          lisp-load-file
          lisp-macroexpand
          lisp-macroexpand-all
          lisp-describe-symbol
          lisp-disassemble-symbol
          lisp-indent-region
          lisp-indent-sexp
          lisp-complete-symbol
          lisp-comment-or-uncomment-region
          lisp-comment-region
          lisp-uncomment-region
          *lisp-repl-mode-keymap*
          lisp-repl-mode
          run-lisp
          lisp-repl-return
          lisp-repl-prev-input
          lisp-repl-next-input
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
                 (defsystem . 1)
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
                    "^(?:[^:*]*:)?\\*[^*]+\\*$"
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
   :syntax-table *lisp-syntax-table*)
  (buffer-put (window-buffer)
              :modeline-format
              (append *modeline-default-format*
                      (list
                       " "
                       (lambda (window)
                         (declare (ignore window))
                         (package-name (lisp-current-package)))))))

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
  (let ((point (point))
        (old-modified-p (buffer-modified-p (window-buffer))))
    (when (sexp-goto-car)
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
              (let ((num-spaces (delete-while-whitespaces t))
                    num-insert-spaces)
                (cond
                 ((and (< 0 (length car-name-str))
                       (or (char= #\( (aref car-name-str 0))
                           (char= #\: (aref car-name-str 0))
                           (char= #\" (aref car-name-str 0))))
                  (setq num-insert-spaces (+ start-col 1)))
                 ((and (null num)
                       (or (eql 0 (search "DEFINE-" car-symbol-name))
                           (eql 0 (search "WITH-" car-symbol-name))
                           (eql 0 (search "DO-" car-symbol-name))))
                  (setq num-insert-spaces (+ start-col 2)))
                 ((null num)
                  (setq num-insert-spaces
                        (or arg-col (+ start-col 1))))
                 ((< (1- argc) num)
                  (setq num-insert-spaces (+ start-col 4)))
                 (t
                  (setq num-insert-spaces (+ start-col 2))))
                (insert-char #\space num-insert-spaces)
                (when (= num-insert-spaces num-spaces)
                  (setf (buffer-modified-p (window-buffer))
                        old-modified-p))))))))))

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

(defun lisp-current-package ()
  (let ((package-name
         (cdr (assoc "package"
                     (buffer-get (window-buffer)
                                 :file-property-list)
                     :test #'equal))))
    (or (and package-name
             (find-package
              (string-upcase package-name)))
        (find-package "COMMON-LISP-USER"))))

(defun lisp-change-package (package)
  (buffer-put (window-buffer)
              :file-property-list
              (acons "package" (package-name package)
                     (buffer-get (window-buffer)
                                 :file-property-list))))

(define-key *lisp-mode-keymap* (kbd "C-x p") 'lisp-read-change-package)
(define-command lisp-read-change-package () ()
  (let* ((package-name
          (string-upcase
           (minibuf-read-line "Package: " ""
                              'complete-package nil)))
         (package (find-package package-name)))
    (cond (package
           (lisp-change-package package) t)
          (t
           (minibuf-print
            (format nil "Package does not exist: ~a"
                    package-name))
           nil))))

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

(defun lisp-eval-in-package (expr package)
  (let* ((string (write-to-string expr))
         (*package* (find-package package)))
    (eval (read-from-string string))))

(defvar *lisp-eval-thread*)
(defvar *lisp-mi-thread*)
(defvar *lisp-eval-thread-values*)
(defvar *lisp-eval-thread-error-p*)

(defun %make-eval-closure (str output-buffer point update-point-p package)
  #'(lambda ()
      (let* ((io (make-buffer-io-stream output-buffer point t))
             (*error-output* io)
             (*trace-output* io)
             (*debug-io* io)
             (*standard-output* io)
             (*standard-input* (make-minibuffer-input-stream))
             (*getch-wait-flag* t))
        (handler-case
            (handler-bind ((error #'lisp-debugger))
              (unwind-protect
                (progn
                  (setq *lisp-eval-thread-values*
                        (multiple-value-list
                         (lisp-eval-in-package
                          (%string-to-exps str)
                          package)))
                  (when update-point-p
                    (point-set
                     (buffer-output-stream-point io))))
                (getch-flush)))
          (error (cdt)
                 (setq *lisp-eval-thread-error-p* t)
                 (setq *lisp-eval-thread-values* (list cdt)))))))

(defun %lisp-mi-thread ()
  (loop :for char := (code-char (cl-charms/low-level:getch)) :do
    (cond ((char= C-g char)
           (bt:interrupt-thread *lisp-eval-thread*
                                #'(lambda () (error "interrupt"))))
          (t
           (ungetch char)))))

(defun eval-string (str output-buffer point
                        &optional
                        update-point-p
                        (package "COMMON-LISP-USER"))
  (setq *lisp-eval-thread-values* nil)
  (setq *lisp-eval-thread-error-p* nil)
  (setq *lisp-eval-thread*
        (bt:make-thread
         (%make-eval-closure str
                             output-buffer
                             point
                             update-point-p
                             package)))
  (setq *lisp-mi-thread* (bt:make-thread #'%lisp-mi-thread))
  (handler-case (bt:join-thread *lisp-eval-thread*)
    #+sbcl
    (sb-thread:join-thread-error (cdt)
                                 (declare (ignore cdt))))
  (bt:destroy-thread *lisp-mi-thread*)
  (values *lisp-eval-thread-values*
          *lisp-eval-thread-error-p*))

(define-key *lisp-mode-keymap* (kbd "M-:") 'lisp-eval-string)
(define-command lisp-eval-string (string) ("sEval: ")
  (let ((output-buffer (get-buffer-create "*output*")))
    (buffer-erase output-buffer)
    (setf (buffer-modified-p output-buffer) nil)
    (prog1 (minibuf-print
            (format nil "~{~s~^,~}"
                    (eval-string string
                                 output-buffer
                                 (point-min)
                                 nil
                                 (lisp-current-package))))
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
  (when (and (cl-fad:file-exists-p filename)
             (not (cl-fad:directory-pathname-p filename)))
    (lisp-eval-string
     (format nil "(load ~s)" filename))))

(defmacro with-safe-form (&body body)
  `(handler-case
       (handler-bind ((error #'lisp-print-error))
         (values (progn ,@body) nil))
     (error (cdt) (values cdt t))))

(defun %lisp-macroexpand (macroexpand-symbol buffer-name)
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
                (lisp-eval-in-package `(,macroexpand-symbol ',expr)
                                      (lisp-current-package)))
          (when (eq (window-buffer)
                    (get-buffer buffer-name))
            (let ((*kill-disable-p* t))
              (kill-sexp))
            (insert-string
             (with-output-to-string (out)
               (pprint expr out)))
            (setq expr (read-from-string
                        (region-string (point-min)
                                       (point-max)))))
          expr))
    (unless error-p
      (lisp-info-popup (get-buffer-create buffer-name)
                       #'(lambda (out)
                           (pprint expr out))))))

(define-key *lisp-mode-keymap* (kbd "C-x m") 'lisp-macroexpand)
(define-command lisp-macroexpand () ()
  (%lisp-macroexpand 'macroexpand-1 "*macroexpand*"))

(define-key *lisp-mode-keymap* (kbd "C-x M") 'lisp-macroexpand-all)
(define-command lisp-macroexpand-all () ()
  (%lisp-macroexpand 'macroexpand "*macroexpand*"))

(defun lisp-read-symbol (prompt)
  (let ((name (minibuf-read-line prompt ""
                                 'complete-symbol
                                 nil)))
    (with-safe-form
      (let ((*package* (lisp-current-package)))
        (read-from-string name)))))

(define-key *lisp-mode-keymap* (kbd "C-x d") 'lisp-describe-symbol)
(define-command lisp-describe-symbol () ()
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Describe: ")
    (unless error-p
      (lisp-info-popup (get-buffer-create "*describe*")
                       #'(lambda (out)
                           (describe name out))))))

(define-key *lisp-mode-keymap* (kbd "C-x M-d") 'lisp-disassemble-symbol)
(define-command lisp-disassemble-symbol () ()
  (block top
    (multiple-value-bind (name error-p)
        (lisp-read-symbol "Disassemble: ")
      (unless error-p
        (let ((str
               (with-output-to-string (out)
                 (handler-case (disassemble name :stream out)
                   (error (condition)
                          (minibuf-print (format nil "~a" condition))
                          (return-from top nil))))))
          (lisp-info-popup (get-buffer-create "*disassemble*")
                           #'(lambda (out)
                               (princ str out))))))))

(defun analyze-symbol (str)
  (let (package
        external-p)
    (let* ((list (split-string str #\:))
           (len (length list)))
      (case len
        ((1)
         (setq str (car list)))
        ((2 3)
         (setq package
               (if (equal "" (car list))
                   (find-package :keyword)
                   (find-package
                    (string-readcase (car list)))))
         (unless package
           (return-from analyze-symbol nil))
         (setq str (car (last list)))
         (if (= len 2)
             (setq external-p t)
             (unless (equal "" (cadr list))
               (return-from analyze-symbol nil))))
        (otherwise
         (return-from analyze-symbol nil))))
    (list package str external-p)))

(defun %collect-symbols (package package-name external-p)
  (let ((symbols))
    (labels ((f (sym separator)
                (push (if package
                          (format nil "~a~a~a"
                                  package-name
                                  separator
                                  (string-downcase (string sym)))
                          (string-downcase (string sym)))
                      symbols)))
      (let ((pkg (or package (lisp-current-package))))
        (if external-p
            (do-external-symbols (sym pkg) (f sym ":"))
            (do-symbols (sym pkg) (f sym "::")))))
    (when (null package)
      (flet ((add (name)
                  (push (format nil "~(~a~):" name) symbols)))
        (dolist (p (list-all-packages))
          (add (package-name p))
          (dolist (p (package-nicknames p))
            (add p)))))
    (nreverse (remove-duplicates symbols :test #'string=))))

(defun complete-symbol (str)
  (let ((result (analyze-symbol str)))
    (when result
      (destructuring-bind (package symbol-name external-p) result
        (declare (ignore symbol-name))
        (let ((package-name
               (subseq str 0 (position #\: str))))
          (completion str
                      (%collect-symbols package
                                        package-name
                                        external-p)))))))

(define-key *lisp-mode-keymap* (kbd "M-C-i") 'lisp-complete-symbol)
(define-command lisp-complete-symbol () ()
  (let* ((end (point))
         (begin (prog2 (backward-sexp)
                    (point)
                  (point-set end)))
         (str (string-left-trim
               "'`," (string-left-trim
                      "#" (region-string begin end)))))
    (multiple-value-bind (comp-str win)
        (popup-completion #'complete-symbol str)
      (when win
          (insert-string
           (subseq comp-str
                   (length str))))))
  t)

(defun lisp-get-arglist (symbol)
  (let ((fstr (make-array '(0)
                          :element-type 'base-char
                          :fill-pointer 0
                          :adjustable t)))
    (with-output-to-string (out fstr)
      (describe symbol out))
    (let ((start-string)
          (end-string))
      #+sbcl
      (progn
        (setq start-string "Lambda-list: (")
        (setq end-string "\\s\\s[A-Z][ a-z]*:"))
      #+ccl
      (progn
        (setq start-string "Arglist: (")
        (setq end-string "\\n[A-Z][a-z]*:"))
      #+(or sbcl ccl)
      (let* ((start (search start-string fstr))
             (end (when start
                    (ppcre:scan end-string fstr :start start))))
        (when (and start end)
          (ppcre:regex-replace-all
           "\\)\\s*\\)"
           (ppcre:regex-replace-all
            "\\s+"
            (format nil "(~a ~a"
                    symbol
                    (string-right-trim
                     '(#\space #\tab)
                     (subseq fstr
                             (+ start (length start-string))
                             end)))
            " ")
           "))"))))))

(define-key *lisp-mode-keymap* (kbd "Spc") 'lisp-self-insert-then-arg-list)
(define-command lisp-self-insert-then-arg-list (n) ("p")
  (prog1 (self-insert n)
    (save-excursion
     (when (sexp-goto-car)
       (let* ((start (point))
              (end (progn (forward-sexp 1 t) (point))))
         (multiple-value-bind (x error-p)
             (ignore-errors
              (values
               (let ((*package* (lisp-current-package)))
                 (read-from-string
                  (region-string start end)))))
           (when (and (not error-p)
                      (symbolp x))
             (let ((arglist (lisp-get-arglist x)))
               (when arglist
                 (minibuf-print arglist))))))))))

(define-key *global-keymap* (kbd "C-x ;") 'lisp-comment-or-uncomment-region)
(define-command lisp-comment-or-uncomment-region (arg) ("P")
  (if arg
      (lisp-uncomment-region)
      (lisp-comment-region)))

(define-command lisp-comment-region () ()
  (save-excursion
   (when (/= (window-cur-linum)
             (save-excursion (skip-chars-forward '(#\space #\tab #\newline))
                             (window-cur-linum)))
     (skip-chars-forward '(#\space #\tab #\newline)))
   (let ((start (region-beginning))
         (end (region-end)))
     (point-set end)
     (skip-chars-forward '(#\space #\tab))
     (unless (eolp)
       (insert-newline 1))
     (point-set start)
     (let ((column (point-column start)))
       (apply-region-lines start end
                           #'(lambda ()
                               (goto-column column)
                               (unless (blank-line-p)
                                 (insert-string ";; "))))))))

(define-command lisp-uncomment-region () ()
  (let ((start (region-beginning))
        (end (region-end)))
    (point-set start)
    (do ()
        ((point<= end (point)))
      (skip-chars-forward '(#\space #\tab))
      (do ((delete-flag nil t))
          ((not (eql #\; (following-char)))
           (when (and delete-flag
                      (syntax-space-char-p (following-char)))
             (delete-char 1 t)))
        (delete-char 1 t))
      (next-line 1))))

(define-key *lisp-mode-keymap* (kbd "C-x z") 'run-lisp)
(define-key *lisp-mode-keymap* (kbd "C-x C-z") 'run-lisp)

(defun lisp-print-values (values)
  (let ((out (make-buffer-output-stream (window-buffer) (point))))
    (dolist (v values)
      (pprint v out))
    (point-set (buffer-output-stream-point out))))

(defvar *lisp-repl-mode-keymap*
  (make-keymap "lisp-repl" nil *lisp-mode-keymap*))

(defvar *lisp-repl-history* nil)

(define-major-mode lisp-repl-mode nil
  (:name "lisp-repl"
   :keymap *lisp-repl-mode-keymap*
   :syntax-table *lisp-syntax-table*)
  (unless *lisp-repl-history*
    (setq *lisp-repl-history* (make-history))))

(define-command run-lisp () ()
  (let ((buffer (get-buffer-create "*lisp-repl*")))
    (setq *current-window* (pop-to-buffer buffer))
    (lisp-repl-mode)
    (lisp-repl-prompt)))

(defun lisp-repl-prompt ()
  (end-of-buffer)
  (unless (bolp)
    (insert-newline))
  (insert-string "> ")
  (buffer-put (window-buffer) :prompt-point (point)))

(defun lisp-repl-paren-correspond-p ()
  (loop :with count := 0 :do
    (insert-string ")")
    (incf count)
    (unless (save-excursion (backward-sexp 1 t))
      (backward-delete-char count t)
      (return (= 1 count)))))

(define-key *lisp-repl-mode-keymap* (kbd "C-m") 'lisp-repl-return)
(define-command lisp-repl-return () ()
  (let ((end (point))
        (buffer (window-buffer)))
    (if (not (lisp-repl-paren-correspond-p))
        (insert-newline)
        (let* ((start (progn
                        (backward-sexp 1 t)
                        (let ((point (point))
                              (prompt-point
                               (buffer-get buffer :prompt-point)))
                          (cond ((point< end prompt-point)
                                 point)
                                ((point< point prompt-point)
                                 prompt-point)
                                (t
                                 point)))))
               (str (region-string start end)))
          (add-history *lisp-repl-history* str)
          (point-set end)
          (insert-newline)
          (multiple-value-bind (values error-p)
              (eval-string str buffer (point))
            (declare (ignore error-p))
            (setq *current-window* (pop-to-buffer buffer))
            (point-set (point-max))
            (lisp-print-values values)
            (lisp-repl-prompt))))))

(define-key *lisp-repl-mode-keymap* (kbd "M-p") 'lisp-repl-prev-input)
(define-command lisp-repl-prev-input () ()
  (multiple-value-bind (str win)
      (prev-history *lisp-repl-history*)
    (when win
      (let ((start (buffer-get (window-buffer) :prompt-point))
            (end (point-max)))
        (let ((*kill-disable-p* t))
          (kill-region start end))
        (insert-string str)))))

(define-key *lisp-repl-mode-keymap* (kbd "M-n") 'lisp-repl-next-input)
(define-command lisp-repl-next-input () ()
  (multiple-value-bind (str win)
      (next-history *lisp-repl-history*)
    (let ((start (buffer-get (window-buffer) :prompt-point))
          (end (point-max)))
      (let ((*kill-disable-p*))
        (kill-region start end))
      (when win
        (insert-string str)))))

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

(defun scratch ()
  (set-buffer (get-buffer-create *scratch-buffer-name*))
  (scratch-mode))

(define-key *scratch-mode-keymap* (kbd "C-j") 'eval-print-last-sexp)
(define-command eval-print-last-sexp () ()
  (let ((point (point)))
    (when (backward-sexp)
      (let ((str (string-trim '(#\newline #\tab #\space)
                              (region-string (point) point))))
        (point-set point)
        (unless (string= str "")
          (insert-newline 1)
          (let ((buffer (window-buffer)))
            (multiple-value-bind (values error-p)
                (eval-string str (window-buffer) (point) t)
              (unless error-p
                (setq *current-window* (pop-to-buffer buffer))
                (lisp-print-values values)))))))))

(defun lisp-print-error (condition)
  (lisp-info-popup (get-buffer-create "*error*")
                   #'(lambda (out)
                       (format out "~a~%~%" condition)
                       (uiop/image:print-backtrace :stream out :count 100))))

(defun ldebug-store-value (condition)
  (do ((x)
       (error-p nil))
      (nil)
    (handler-case
        (setq x
              (eval
               (read-from-string
                (minibuf-read-string
                 "Type a form to be evaluated: ")
                nil)))
      (error (cdt)
             (setq error-p t)
             (minibuf-print (format nil "~a" cdt))))
    (unless error-p
      (store-value x condition)
      (return))))

(defun lisp-debugger (condition)
  (let* ((choices (compute-restarts condition))
         (n (length choices)))
    (lisp-info-popup (get-buffer-create "*error*")
                     #'(lambda (out)
                         (format out "~a~%~%" condition)
                         (loop
                           for choice in choices
                           for i from 1
                           do (format out "~&[~d] ~a~%" i choice))
                         (terpri out)
                         (uiop/image:print-backtrace :stream out :count 100)))
    (loop
      (window-update-all)
      (let* ((str (minibuf-read-string "Debug: "))
             (i (parse-integer str :junk-allowed t)))
        (cond ((and i (<= 1 i n))
               (let ((restart (nth (1- i) choices)))
                 (cond ((eq 'store-value (restart-name restart))
                        (ldebug-store-value condition))
                       (t (invoke-restart-interactively restart))))
               (return))
              (t
               (let ((x
                      (handler-case (eval (read-from-string str nil))
                        (error (cdt)
                               (format nil "~a" cdt)))))
                 (info-popup (get-buffer-create "*output*")
                             #'(lambda (out)
                                 (princ x out)))))))))
  condition)

(defun lisp-info-popup (buffer &optional fn)
  (funcall (info-popup-closure 'lisp-mode)
           buffer fn t))

(setq *auto-mode-alist*
      (append '((".lisp$" . lisp-mode)
                (".asd$" . lisp-mode))
              *auto-mode-alist*))
