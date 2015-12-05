;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*lisp-indent-table*
          *lisp-mode-keymap*
          *lisp-syntax-table*
          lisp-mode
          lisp-indent-line
          lisp-newline-and-indent
          lisp-indent-region
          lisp-indent-sexp
          lisp-beginning-of-defun
          lisp-end-of-defun
          lisp-current-package
          lisp-set-package
          lisp-eval-string
          lisp-eval-region
          lisp-eval-defun
          lisp-eval-last-sexp
          lisp-load-file
          lisp-macroexpand
          lisp-macroexpand-all
          lisp-describe-symbol
          lisp-disassemble-symbol
          #+sbcl lisp-find-definitions
          complete-symbol
          lisp-complete-symbol
          lisp-get-arglist
          lisp-echo-arglist
          lisp-self-insert-then-arg-list
          lisp-comment-or-uncomment-region
          lisp-comment-region
          lisp-uncomment-region
          *lisp-repl-mode-keymap*
          lisp-repl-mode
          run-lisp
          lisp-repl-prompt
          lisp-repl-return
          lisp-repl-prev-input
          lisp-repl-next-input
          lisp-repl-reset
          lisp-repl-set-package
          *scratch-mode-keymap*
          scratch-mode
          scratch
          eval-print-last-sexp
          lisp-info-popup))

(defvar *lisp-indent-table* (make-hash-table :test 'equal))

(loop
  :for (name . n)
  :in '(("block" . 1)
        ("case" . 1)
        ("ccase" . 1)
        ("ecase" . 1)
        ("typecase" . 1)
        ("etypecase" . 1)
        ("ctypecase" . 1)
        ("catch" . 1)
        ("defclass" . 2)
        ("define-condition" . 2)
        ("define-modify-macro" . 1)
        ("defsetf" . 2)
        ("defun" . 2)
        ("define-setf-method" . 2)
        ("define-setf-expander" . 2)
        ("defmacro" . 2)
        ("deftype" . 2)
        ("defmethod" . 2)
        ("defpackage" . 1)
        ("defstruct" . 1)
        ("destructuring-bind" . 2)
        ("do" . 2)
        ("do*" . 2)
        ("dolist" . 1)
        ("dotimes" . 1)
        ("eval-when" . 1)
        ("flet" . 1)
        ("labels" . 1)
        ("macrolet" . 1)
        ("generic-flet" . 1)
        ("generic-labels" . 1)
        ("handler-case" . 1)
        ("restart-case" . 1)
        ("lambda" . 1)
        ("let" . 1)
        ("let*" . 1)
        ("handler-bind" . 1)
        ("restart-bind" . 1)
        ("locally" . 0)
        ("multiple-value-bind" . 2)
        ("multiple-value-call" . 1)
        ("multiple-value-prog1" . 1)
        ("prog" . 1)
        ("prog*" . 1)
        ("prog1" . 1)
        ("prog2" . 2)
        ("progn" . 0)
        ("progv" . 2)
        ("return" . 0)
        ("return-from" . 1)
        ("symbol-macrolet" . 1)
        ("tagbody" . 0)
        ("throw" . 1)
        ("unless" . 1)
        ("unwind-protect" . 0)
        ("when" . 1)
        ("with-accessors" . 2)
        ("with-condition-restarts" . 2)
        ("with-output-to-string" . 1)
        ("with-slots" . 2)
        ("with-standard-io-syntax" . 2)
        ("defparameter" . 1)
        ("defvar" . 1)
        ("defconstant" . 1)
        ("defsystem" . 1)
        ("loop" . 0))
  :do (setf (gethash name *lisp-indent-table*) n))

(defvar *lisp-mode-keymap*
  (make-keymap "lisp"))

(defvar *lisp-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\> #\= #\/ #\: #\.)
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
              :enable-syntax-highlight t)
  (buffer-put (window-buffer)
              :modeline-format
              (append *modeline-default-format*
                      (list
                       " "
                       (lambda (window)
                         (package-name (lisp-current-package
                                        (window-buffer window))))))))

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

(defun sexp-goto-car () ()
  (let ((point (point)))
    (do ((end-linum (point-linum point)))
        ((let ((point (point)))
           (if (backward-sexp 1 t)
               (point= point (point))
               t))
         t)
      (let ((start-linum (point-linum (point))))
        (when (< 100 (- end-linum start-linum))
          (point-set point)
          (return nil))))))

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
            (let ((num (gethash (string-downcase car-symbol-name)
                                *lisp-indent-table*)))
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
                (when (eql num-insert-spaces num-spaces)
                  (setf (buffer-modified-p (window-buffer))
                        old-modified-p)))))))))
  t)

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

(define-key *global-keymap* (kbd "M-C-a") 'lisp-beginning-of-defun)
(define-command lisp-beginning-of-defun (&optional (n 1)) ("p")
  (beginning-of-defun-abstract n #'(lambda () (looking-at "^\\("))))

(define-key *global-keymap* (kbd "M-C-e") 'lisp-end-of-defun)
(define-command lisp-end-of-defun (&optional (n 1)) ("p")
  (if (minusp n)
      (lisp-beginning-of-defun (- n))
      (dotimes (_ n t)
        (down-list 1 t)
        (lisp-beginning-of-defun 1)
        (unless (forward-sexp 1)
          (return nil))
        (loop
          for c = (following-char)
          do (cond ((char= c #\newline)
                    (return (and (next-line 1)
                                 (beginning-of-line))))
                   ((syntax-space-char-p c)
                    (unless (next-char 1)
                      (return nil)))
                   (t
                    (return t)))))))

(defun lisp-buffer-package (buffer)
  (let ((package-name
         (cdr (assoc "package"
                     (buffer-get buffer
                                 :file-property-list)
                     :test #'equal))))
    (when package-name
      (string-upcase package-name))))

(defun lisp-current-package (&optional (buffer (window-buffer)))
  (or (find-package (lisp-buffer-package buffer))
      (find-package "COMMON-LISP-USER")))

(defun lisp-change-package (package)
  (buffer-put (window-buffer)
              :file-property-list
              (acons "package" (package-name package)
                     (buffer-get (window-buffer)
                                 :file-property-list))))

(defun lisp-read-change-package (find-package-function
                                 complete-package-function)
  (let* ((package-name
          (string-upcase
           (minibuf-read-line "Package: " ""
                              complete-package-function nil)))
         (package (funcall find-package-function package-name)))
    (cond (package
           (lisp-change-package package) t)
          (t
           (minibuf-print
            (format nil "Package does not exist: ~a"
                    package-name))
           nil))))

(define-key *lisp-mode-keymap* (kbd "C-x p") 'lisp-set-package)
(define-command lisp-set-package () ()
  (lisp-read-change-package
   #'find-package
   #'(lambda (str)
       (completion str
                   (mapcar #'(lambda (pkg)
                               (string-downcase (package-name pkg)))
                           (list-all-packages))))))

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
    (multiple-value-list (eval (read-from-string string)))))

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
      (let* ((str (catch 'abort (minibuf-read-string "Debug: ")))
             (i (and (stringp str) (parse-integer str :junk-allowed t))))
        (cond ((eq str 'abort))
              ((and i (<= 1 i n))
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

(defun eval-string-internal (string output-buffer point
                                  &optional
                                  update-point-p (package "COMMON-LISP-USER"))
  (let* ((error-p)
         (results)
         (io (make-buffer-io-stream output-buffer point t))
         (*terminal-io* io)
         (*standard-output* io)
         (*standard-input* io)
         (*error-output* io)
         (*query-io* io)
         (*debug-io* io)
         (*trace-output* io))
    (handler-case
        (handler-bind ((error #'lisp-debugger))
          (setq results
                (restart-case (lisp-eval-in-package
                               (%string-to-exps string)
                               package)
                  (abort () :report "Abort.")))
          (when update-point-p
            (point-set
             (buffer-output-stream-point io))))
      (error (condition)
             (setq error-p t)
             (setq results (list condition))))
    (values results error-p)))

(defun eval-string (string output-buffer point
                           &optional
                           update-point-p (package "COMMON-LISP-USER"))
  (setq *getch-wait-p* t)
  (let* ((main-thread (bt:current-thread))
         (input-thread
          (bt:make-thread #'(lambda ()
                              (ignore-errors
                               (loop :for c := (charms/ll:getch) :do
                                 (cond ((= c -1))
                                       ((eql c (char-code C-g))
                                        (bt:interrupt-thread
                                         main-thread
                                         #'(lambda ()
                                             (error "interrupt"))))
                                       (t
                                        (ungetch (code-char c))))))))))
    (multiple-value-bind (results error-p)
        (eval-string-internal string
                              output-buffer
                              point
                              update-point-p
                              package)
      (bt:interrupt-thread input-thread
                           #'(lambda () (error "error")))
      (setq *getch-wait-p* nil)
      (values results error-p))))

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

(define-key *lisp-mode-keymap* (kbd "C-x r") 'lisp-eval-region)
(define-command lisp-eval-region (&optional begin end) ("r")
  (unless (or begin end)
    (setq begin (region-beginning))
    (setq end (region-end)))
  (buffer-mark-cancel (window-buffer))
  (lisp-eval-string (region-string begin end))
  t)

(defun lisp-move-and-eval-sexp (move-sexp eval-string-function)
  (let ((str (save-excursion
              (and (funcall move-sexp)
                   (mark-sexp)
                   (region-string (region-beginning) (region-end))))))
    (when str
      (funcall eval-string-function str)
      t)))

(define-key *lisp-mode-keymap* (kbd "M-C-x") 'lisp-eval-defun)
(define-command lisp-eval-defun () ()
  (lisp-move-and-eval-sexp #'top-of-defun #'lisp-eval-string))

(define-key *global-keymap* (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-command lisp-eval-last-sexp () ()
  (lisp-move-and-eval-sexp #'backward-sexp #'lisp-eval-string))

(define-key *lisp-mode-keymap* (kbd "C-x l") 'lisp-load-file)
(define-key *lisp-mode-keymap* (kbd "C-x C-l") 'lisp-load-file)
(define-command lisp-load-file (filename) ("fLoad File: ")
  (when (and (cl-fad:file-exists-p filename)
             (not (cl-fad:directory-pathname-p filename)))
    (lisp-eval-string
     (format nil "(load ~s)" filename))))

(defun lisp-print-error (condition)
  (lisp-info-popup (get-buffer-create "*error*")
                   #'(lambda (out)
                       (format out "~a~%~%" condition)
                       (uiop/image:print-backtrace :stream out :count 100))))

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
                (car
                 (lisp-eval-in-package `(,macroexpand-symbol ',expr)
                                       (lisp-current-package))))
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
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Disassemble: ")
    (unless error-p
      (let ((str
             (with-output-to-string (out)
               (handler-case (disassemble name :stream out)
                 (error (condition)
                        (minibuf-print (format nil "~a" condition))
                        (return-from lisp-disassemble-symbol nil))))))
        (lisp-info-popup (get-buffer-create "*disassemble*")
                         #'(lambda (out)
                             (princ str out)))))))


#+sbcl
(progn
  (defparameter *lisp-definition-types*
    '(:variable
      :constant
      :type
      :symbol-macro
      :macro
      :compiler-macro
      :function
      :generic-function
      :method
      :setf-expander
      :structure
      :condition
      :class
      :method-combination
      :package
      :transform
      :optimizer
      :vop
      :source-transform
      :ir1-convert
      :declaration
      :alien-type))

  (defun %collect-definitions (name)
    (loop :for type :in *lisp-definition-types*
      :append (sb-introspect:find-definition-sources-by-name
               name type)))

  (defun collect-definitions (name)
    (sort
     (loop :for def :in (%collect-definitions name)
       :collect
       (list
        (sb-introspect:definition-source-pathname def)
        (1+
         (car
          (sb-introspect:definition-source-form-path
           def)))))
     #'< :key #'second))

  (define-key *lisp-mode-keymap* (kbd "M-.") 'lisp-find-definitions)
  (define-command lisp-find-definitions () ()
    (multiple-value-bind (name error-p)
        (lisp-read-symbol "Find definitions: ")
      (unless error-p
        (let ((defs
               (loop :for (pathname form-path)
                 :in (collect-definitions name)
                 :collect (list (namestring pathname)
                                #'(lambda ()
                                    (beginning-of-buffer)
                                    (when (forward-sexp form-path)
                                      (backward-sexp 1)))))))
          (update-grep-list
           defs
           #'(lambda ()
               (info-popup (get-buffer-create "*Definitions*")
                           #'(lambda (out)
                               (loop :for (filename _) :in defs :do
                                 (format out "~a~%" filename)))
                           nil))))))))

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

(defun lisp-preceding-symbol ()
  (let* ((end (point))
         (begin (prog2 (backward-sexp)
                    (point)
                  (point-set end)))
         (str (string-left-trim
               "'`," (string-left-trim
                      "#" (region-string begin end)))))
    str))

(defun lisp-popup-completion-symbol (complete-function)
  (let ((str (lisp-preceding-symbol)))
    (multiple-value-bind (comp-str win)
        (popup-completion complete-function str)
      (when win
        (insert-string
         (subseq comp-str
                 (length str)))))))

(define-key *lisp-mode-keymap* (kbd "M-C-i") 'lisp-complete-symbol)
(define-command lisp-complete-symbol () ()
  (lisp-popup-completion-symbol #'complete-symbol)
  t)

#+ccl
(defun lisp-get-arglist (symbol)
  (when (fboundp symbol)
    (format nil "~a" (ccl:arglist symbol))))

#+sbcl
(defun lisp-get-arglist (symbol)
  (when (fboundp symbol)
    (ppcre:regex-replace-all
     "\\s+"
     (format nil "~a"
             (sb-introspect:function-lambda-list symbol))
     " ")))

(defun lisp-echo-arglist (get-arglist-function)
  (save-excursion
   (when (sexp-goto-car)
     (let* ((start (point))
            (end (progn (forward-sexp 1 t) (point))))
       (multiple-value-bind (arglist)
           (funcall get-arglist-function
                    (region-string start end))
         (when arglist
           (minibuf-print arglist)))))))

(define-key *lisp-mode-keymap* (kbd "Spc") 'lisp-self-insert-then-arg-list)
(define-command lisp-self-insert-then-arg-list (n) ("p")
  (prog1 (self-insert n)
    (lisp-echo-arglist
     #'(lambda (string)
         (multiple-value-bind (x error-p)
             (ignore-errors
              (destructuring-bind (package symbol-name external-p)
                  (analyze-symbol string)
                (declare (ignore external-p))
                (multiple-value-bind (symbol status)
                    (intern (string-readcase symbol-name)
                            (or package
                                (lisp-current-package)))
                  (cond ((null status)
                         (unintern symbol)
                         nil)
                        (t
                         symbol)))))
           (when (and (not error-p)
                      (symbolp x))
             (lisp-get-arglist x)))))))

(define-key *lisp-mode-keymap* (kbd "C-x ;") 'lisp-comment-or-uncomment-region)
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

(defun shorten-package-name (package)
  (car
   (sort (copy-list
          (cons (package-name package)
                (package-nicknames package)))
         #'(lambda (x y)
             (< (length x) (length y))))))

(defun lisp-repl-prompt ()
  (end-of-buffer)
  (unless (bolp)
    (insert-newline))
  (insert-string (format nil "~a> "
                         (shorten-package-name
                          (lisp-current-package))))
  (buffer-put-attribute (window-buffer)
                        (make-point (window-cur-linum) 0)
                        (make-point (window-cur-linum) (window-cur-col))
                        (make-attr :bold-p t :color :blue))
  (buffer-put (window-buffer) :prompt-point (point))
  (buffer-undo-boundary (window-buffer)))

(defun lisp-repl-paren-correspond-p ()
  (loop :with count := 0 :do
    (insert-string ")")
    (incf count)
    (unless (save-excursion (backward-sexp 1 t))
      (backward-delete-char count t)
      (return (= 1 count)))))

(define-key *lisp-repl-mode-keymap* (kbd "C-m") 'lisp-repl-return)
(define-command lisp-repl-return () ()
  (end-of-buffer)
  (let ((end (point))
        (buffer (window-buffer)))
    (if (not (lisp-repl-paren-correspond-p))
        (insert-newline)
        (let ((start (buffer-get buffer :prompt-point)))
          (unless (point< start end)
            (lisp-repl-reset nil)
            (return-from lisp-repl-return t))
          (let ((str (region-string start end)))
            (add-history *lisp-repl-history* str)
            (point-set end)
            (insert-newline)
            (multiple-value-bind (values error-p)
                (eval-string str buffer (point) t (lisp-current-package))
              (declare (ignore error-p))
              (setq *current-window* (pop-to-buffer buffer))
              (point-set (point-max))
              (lisp-print-values values))
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

(define-key *lisp-repl-mode-keymap* (kbd "M-r") 'lisp-repl-reset)
(define-command lisp-repl-reset (arg) ("P")
  (when arg
    (let ((*kill-disable-p*))
      (kill-region (point-min) (point-max))))
  (lisp-repl-prompt)
  t)

(define-key *lisp-repl-mode-keymap* (kbd "C-x p") 'lisp-repl-set-package)
(define-command lisp-repl-set-package () ()
  (lisp-set-package)
  (lisp-repl-prompt)
  t)

(defvar *scratch-mode-keymap*
  (make-keymap "scratch" nil *lisp-mode-keymap*))

(define-major-mode scratch-mode nil
  (:name "scratch"
   :keymap *scratch-mode-keymap*
   :syntax-table *lisp-syntax-table*))

(define-command scratch () ()
  (set-buffer (get-buffer-create "*scratch*"))
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

(defun lisp-info-popup (buffer &optional fn)
  (funcall (info-popup-closure 'lisp-mode)
           buffer fn t))

(setq *auto-mode-alist*
      (append '((".lisp$" . lisp-mode)
                (".asd$" . lisp-mode))
              *auto-mode-alist*))
