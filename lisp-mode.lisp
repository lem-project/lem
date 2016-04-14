;; -*- Mode: LISP; Package: LEM.LISP-MODE -*-

(in-package :cl-user)
(defpackage :lem.lisp-mode
  (:use :cl :lem :lem.grep)
  (:import-from
   :lem.util
   :completion)
  (:export
   :*lisp-indent-table*
   :*lisp-mode-keymap*
   :*lisp-syntax-table*
   :lisp-mode
   :lisp-newline-and-indent
   :lisp-indent-sexp
   :lisp-beginning-of-defun
   :lisp-end-of-defun
   :lisp-current-package
   :lisp-set-package
   :lisp-eval-string
   :lisp-eval-region
   :lisp-eval-defun
   :lisp-eval-last-sexp
   :lisp-load-file
   :lisp-macroexpand
   :lisp-macroexpand-all
   :lisp-describe-symbol
   :lisp-disassemble-symbol
   :lisp-find-definitions
   :complete-symbol
   :lisp-complete-symbol
   :lisp-get-arglist
   :lisp-echo-arglist
   :lisp-self-insert-then-arg-list
   :lisp-comment-or-uncomment-region
   :lisp-comment-region
   :lisp-uncomment-region
   :lisp-eval-print-last-sexp
   :*lisp-repl-mode-keymap*
   :lisp-repl-mode
   :start-lisp-repl
   :lisp-repl-get-prompt
   :lisp-repl-paren-correspond-p
   :lisp-repl-confirm
   :lisp-repl-set-package
   :lisp-info-popup))
(in-package :lem.lisp-mode)

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
        ("with-open-file" . 1)
        ("with-slots" . 2)
        ("with-standard-io-syntax" . 2)
        ("defparameter" . 1)
        ("defvar" . 1)
        ("defconstant" . 1)
        ("defsystem" . 1)
        ("loop" . 0))
  :do (setf (gethash name *lisp-indent-table*) n))

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

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "(")
                  :matched-symbol :start-expr
                  :symbol-lifetime 1)

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "[^() \\t]+" :regex-p t)
                  :test-symbol :define-start
                  :attr :function-name-attr)

(dolist (str '("defun"
               "defclass"
               "defgeneric"
               "defsetf"
               "defmacro"
               "deftype"
               "defmethod"
               "defpackage"
               "defstruct"
               "defvar"
               "defparameter"
               "defconstant"))
  (syntax-add-match *lisp-syntax-table*
                    (make-syntax-test str :word-p t)
                    :test-symbol :start-expr
                    :attr :keyword-attr
                    :matched-symbol :define-start
                    :symbol-lifetime 1))

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "^(:?[^: \\t]+:)?define-[^ \\t()]*$"
                                    :regex-p t :word-p t)
                  :test-symbol :start-expr
                  :attr :keyword-attr
                  :matched-symbol :define-start
                  :symbol-lifetime 1)

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
               "with-open-file"
               "with-output-to-string"
               "with-slots"
               "with-standard-io-syntax"
               "loop"
               "declare"
               "declaim"
               "proclaim"))
  (syntax-add-match *lisp-syntax-table*
                    (make-syntax-test str :word-p t)
                    :test-symbol :start-expr
                    :attr :keyword-attr))

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "^(?:[^:*]*:)?\\*[^*]+\\*$"
                                    :regex-p t :word-p t)
                  :attr :variable-attr)

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "^:[^() \\t]+$"
                                    :regex-p t :word-p t)
                  :attr :constant-attr)

(syntax-add-match *lisp-syntax-table*
                  (make-syntax-test "^&[^() \\t]+$"
                                    :regex-p t :word-p t)
                  :attr :constant-attr)

(define-major-mode lisp-mode prog-mode
  (:name "lisp"
   :keymap *lisp-mode-keymap*
   :syntax-table *lisp-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :indent-tabs-mode) nil)
  (setf (get-bvar :modeline-format)
        (append *modeline-default-format*
                (list
                 " "
                 (lambda (window)
                   (package-name (lisp-current-package
                                  (window-buffer window)))))))
  (setf (get-bvar :calc-indent-function)
        'lisp-calc-indent))

(defun %lisp-mode-skip-expr-prefix (c1 c2 step-fn)
  (when c1
    (multiple-value-bind (unused-fn dispatch-char-p)
        (get-macro-character c1)
      (declare (ignore unused-fn))
      (when (and dispatch-char-p
                 (not (eql c2 #\())
                 (get-dispatch-macro-character c1 c2))
        (funcall step-fn c1 c2)))))

(defvar *lisp-mode-skip-features-sharp-macro-p* nil)

(defun lisp-mode-skip-expr-prefix-forward ()
  (%lisp-mode-skip-expr-prefix
   (char-after 0) (char-after 1)
   #'(lambda (c1 c2)
       (declare (ignore c1 c2))
       (cond ;; (*lisp-mode-skip-features-sharp-macro-p*
             ;;  (when (and (eql c1 #\#) (member c2 '(#\+ #\-)))
             ;;    (next-char 2))
             ;;  (if (eql #\( (following-char))
             ;;      (skip-list-forward 0)
             ;;      (skip-symbol-forward))
             ;;  (skip-chars-forward '(#\space #\tab #\newline)))
             (t
              (next-char 2))))))

(defun lisp-mode-skip-expr-prefix-backward ()
  (%lisp-mode-skip-expr-prefix (char-before 2)
                               (char-before 1)
                               #'(lambda (c1 c2)
                                   (declare (ignore c1 c2))
                                   (prev-char 2))))

(defun sexp-goto-car (limit-linum)
  (let ((point (current-point)))
    (do ((end-linum (point-linum point)))
        ((let ((point (current-point)))
           (if (backward-sexp 1 t)
               (point= point (current-point))
               t))
         t)
      (let ((start-linum (point-linum (current-point))))
        (when (< limit-linum (- end-linum start-linum))
          (point-set point)
          (return nil))))))

(defun lisp-looking-at-word ()
  (save-excursion
   (skip-chars-forward
    #'(lambda (c)
        (or (eq c #\space)
            (eq c #\tab))))
   (let ((begin (current-point)))
     (forward-sexp)
     (list (region-string begin (current-point))
           (when (= (current-linum)
                    (progn
                      (skip-chars-forward 'syntax-space-char-p)
                      (current-linum)))
             (current-charpos))))))

(defun %count-sexps (goal)
  (do ((count 0 (1+ count)))
      ((or (not (forward-sexp 1 t))
           (eobp)
           (point< goal (current-point)))
       count)))

(defun lisp-calc-indent ()
  (let (num-insert-spaces)
    (save-excursion
     (let ((point (progn
                    (beginning-of-line)
                    (current-point))))
       (when (save-excursion (and (backward-sexp 1 t) (bolp)))
         (return-from lisp-calc-indent 0))
       (when (sexp-goto-car 2000)
         (let ((start-pos (1- (current-charpos)))
               (not-list-p (save-excursion
                            (search-backward "(")
                            (member (preceding-char) '(#\#)))))
           (destructuring-bind (car-name-str arg-pos)
               (lisp-looking-at-word)
             (let* ((car-symbol-name
                     (string-upcase
                      (car
                       (last (uiop:split-string car-name-str :separator ":")))))
                    (argc (%count-sexps point)))
               (let ((num (gethash (string-downcase car-symbol-name)
                                   *lisp-indent-table*)))
                 (point-set point)
                 (cond
                  ((or not-list-p
                       (and (null num)
                            (< 0 (length car-name-str))
                            (or (char= #\( (aref car-name-str 0))
                                (char= #\: (aref car-name-str 0))
                                (char= #\" (aref car-name-str 0)))))
                   (setq num-insert-spaces (+ start-pos 1)))
                  ((and (null num)
                        (or (eql 0 (search "DEFINE-" car-symbol-name))
                            (eql 0 (search "WITH-" car-symbol-name))
                            (eql 0 (search "DO-" car-symbol-name))))
                   (setq num-insert-spaces (+ start-pos 2)))
                  ((null num)
                   (setq num-insert-spaces
                         (or arg-pos (+ start-pos 1))))
                  ((< (1- argc) num)
                   (setq num-insert-spaces (+ start-pos 4)))
                  (t
                   (setq num-insert-spaces (+ start-pos 2)))))))))))
    num-insert-spaces))

(define-key *lisp-mode-keymap* (kbd "M-C-q") 'lisp-indent-sexp)
(define-command lisp-indent-sexp () ()
  (and (mark-sexp)
       (prog-indent-region)))

(define-key *global-keymap* (kbd "M-C-a") 'lisp-beginning-of-defun)
(define-command lisp-beginning-of-defun (&optional (n 1)) ("p")
  (beginning-of-defun-abstract n #'(lambda () (looking-at-line "^\\("))))

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
                    (return (and (forward-line 1)
                                 (beginning-of-line))))
                   ((syntax-space-char-p c)
                    (unless (next-char 1)
                      (return nil)))
                   (t
                    (return t)))))))

(defun lisp-buffer-package (buffer)
  (let ((package-name
         (cdr (assoc "package"
                     (get-bvar :file-property-list
                               :buffer buffer)
                     :test #'equal))))
    (when package-name
      (string-upcase package-name))))

(defun lisp-current-package (&optional (buffer (current-buffer)))
  (or (find-package (lisp-buffer-package buffer))
      (find-package "COMMON-LISP-USER")))

(defun lisp-change-package (package)
  (setf (get-bvar :file-property-list)
        (acons "package" (package-name package)
               (get-bvar :file-property-list))))

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

(defun %string-to-exps (str package)
  (let ((str str)
        (exps)
        (eof-value (make-symbol "eof")))
    (do ()
        ((string= "" str))
      (multiple-value-bind (expr i)
          (let ((*package* package))
            (read-from-string str nil eof-value))
        (when (eq expr eof-value)
          (return))
        (push expr exps)
        (setq str (subseq str i))))
    (nreverse exps)))

(defun string-readcase (string &key (start 0) end)
  (ecase (readtable-case *readtable*)
    ((:upcase)
     (string-upcase string :start start :end end))
    ((:downcase)
     (string-downcase string :start start :end end))
    ((:invert :preserve)
     string)))

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
  (lem::raw)
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
                         (uiop/image:print-backtrace :stream out :count 100))
                     nil)
    (loop
      (redraw-screen)
      (handler-case
          (let* ((str (minibuf-read-string "Debug: "))
                 (i (and (stringp str) (parse-integer str :junk-allowed t))))
            (cond ((and i (<= 1 i n))
                   (let ((restart (nth (1- i) choices)))
                     (lem::noraw)
                     (cond ((eq 'store-value (restart-name restart))
                            (ldebug-store-value condition))
                           (t
                            (invoke-restart-interactively restart))))
                   (return))
                  (t
                   (lem::noraw)
                   (let ((x
                          (handler-case (eval (read-from-string str nil))
                            (error (cdt) (format nil "~a" cdt)))))
                     (info-popup (get-buffer-create "*output*")
                                 #'(lambda (out)
                                     (princ x out))
                                 nil))
                   (lem::raw))))
        (editor-abort ()))))
  condition)

(defun %lisp-eval-internal (x output-buffer point &optional update-point-p)
  (let* ((error-p)
         (results)
         (io (make-editor-io-stream output-buffer point t))
         (*terminal-io* io)
         (*standard-output* io)
         (*standard-input* io)
         (*error-output* io)
         (*query-io* io)
         (*debug-io* io)
         (*trace-output* io))
    (handler-case-bind (#'lisp-debugger
                        (setq results
                              (restart-case
                                  (multiple-value-list (eval x))
                                (editor-abort () :report "Abort.")))
                        (when update-point-p
                          (point-set
                           (buffer-output-stream-point io))))
                       ((condition)
                        (setq error-p t)
                        (setq results (list condition))))
    (values results error-p)))

(defun %lisp-eval (x output-buffer point
                     &optional update-point-p)
  (unless point (setq point (point-min)))
  (lem::noraw)
  (unwind-protect
    (multiple-value-bind (results error-p)
        (%lisp-eval-internal x
                             output-buffer
                             point
                             update-point-p)
      (values results error-p))
    (lem::raw)))

(defun %lisp-eval-string (string output-buffer point
                                 &optional
                                 update-point-p (package "COMMON-LISP-USER"))
  (%lisp-eval `(cl:progn ,@(%string-to-exps string package))
              output-buffer
              point
              update-point-p))

(define-key *lisp-mode-keymap* (kbd "M-:") 'lisp-eval-string)
(define-command lisp-eval-string (string) ("sEval: ")
  (let ((output-buffer (get-buffer-create "*output*")))
    (buffer-erase output-buffer)
    (setf (buffer-modified-p output-buffer) nil)
    (prog1 (minibuf-print
            (format nil "~{~s~^,~}"
                    (%lisp-eval-string string output-buffer nil nil
                                       (lisp-current-package))))
      (when (buffer-modified-p output-buffer)
        (lisp-info-popup output-buffer
                         nil
                         nil)))))

(define-key *lisp-mode-keymap* (kbd "C-x r") 'lisp-eval-region)
(define-command lisp-eval-region (&optional begin end) ("r")
  (unless (or begin end)
    (setq begin (region-beginning))
    (setq end (region-end)))
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

(define-key *lisp-mode-keymap* (kbd "C-x C-e") 'lisp-eval-last-sexp)
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
                       (uiop/image:print-backtrace :stream out :count 100))
                   nil))

(defmacro with-safe-form (&body body)
  `(handler-case
       (handler-bind ((error #'lisp-print-error))
         (values (progn ,@body) nil))
     (error (cdt) (values cdt t))))

(defun %lisp-macroexpand-at-point (macroexpand-symbol)
  (car (lisp-eval-in-package
        `(,macroexpand-symbol
          (let ((*package* (lisp-current-package)))
            (read-from-string
             (region-string
              (current-point)
              (save-excursion (forward-sexp 1)
                              (current-point))))))
        (lisp-current-package))))

(defun %lisp-macroexpand-replace-expr (expr)
  (let ((*kill-disable-p* t))
    (kill-sexp))
  (pprint expr (make-buffer-output-stream))
  (read-from-string
   (region-string (point-min)
                  (point-max))))

(defun %lisp-macroexpand (macroexpand-symbol buffer-name)
  (multiple-value-bind (expr error-p)
      (with-safe-form
        (let ((expr (%lisp-macroexpand-at-point macroexpand-symbol)))
          (cond ((eq (current-buffer)
                     (get-buffer buffer-name))
                 (%lisp-macroexpand-replace-expr expr))
                (t
                 expr))))
    (unless error-p
      (lisp-info-popup (get-buffer-create buffer-name)
                       #'(lambda (out) (pprint expr out))
                       t))))

(define-key *lisp-mode-keymap* (kbd "C-x m") 'lisp-macroexpand)
(define-command lisp-macroexpand () ()
  (%lisp-macroexpand 'macroexpand-1 "*macroexpand*"))

(define-key *lisp-mode-keymap* (kbd "C-x M") 'lisp-macroexpand-all)
(define-command lisp-macroexpand-all () ()
  (%lisp-macroexpand 'macroexpand "*macroexpand*"))

(defun lisp-looking-at-symbol-name ()
  (let ((not-symbol-elements '(#\( #\) #\space #\tab #\newline)))
    (flet ((symbol-char-p (c) (not (member c not-symbol-elements))))
      (when (or (symbol-char-p (following-char))
                (symbol-char-p (preceding-char)))
        (save-excursion
         (skip-chars-backward not-symbol-elements t)
         (mark-sexp)
         (region-string (region-beginning) (region-end)))))))

(defun lisp-read-symbol (prompt &optional (confirm-p t))
  (let ((default-name (or (lisp-looking-at-symbol-name) "")))
    (let ((name (if confirm-p
                    (minibuf-read-line prompt
                                       default-name
                                       'complete-symbol
                                       nil)
                    default-name)))
      (with-safe-form
        (let ((*package* (lisp-current-package)))
          (read-from-string name))))))

(define-key *lisp-mode-keymap* (kbd "C-x d") 'lisp-describe-symbol)
(define-command lisp-describe-symbol () ()
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Describe: ")
    (unless error-p
      (lisp-info-popup (get-buffer-create "*describe*")
                       #'(lambda (out)
                           (describe name out))
                       nil))))

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
                             (princ str out))
                         nil)))))

(defvar *lisp-find-definition-stack* nil)

(defun find-definitions (name)
  (let ((swank::*buffer-package* (lisp-current-package))
        (swank::*buffer-readtable* *readtable*))
    (swank::find-definitions name)))

(defun lisp-find-definitions-internal (symbol)
  (let ((moves))
    (dolist (definition (find-definitions symbol))
      (destructuring-bind (head &rest alist) definition
        (let ((location (cdr (assoc :location alist))))
          (when location
            (let ((file (second (assoc :file location)))
                  (position (second (assoc :position location))))
              (push (list head file position) moves))))))
    (nreverse moves)))

(define-key *lisp-mode-keymap* (kbd "M-.") 'lisp-find-definitions)
(define-command lisp-find-definitions () ()
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Find definitions: ")
    (unless error-p
      (let ((defs))
        (dolist (def (lisp-find-definitions-internal name))
          (destructuring-bind (head file filepos) def
            (declare (ignore head))
            (push (list file
                        (lambda ()
                          (find-file file)
                          (beginning-of-buffer)
                          (next-char filepos)
                          (redraw-screen)))
                  defs)))
        (push (cons (current-buffer) (current-point))
              *lisp-find-definition-stack*)
        (cond ((= 1 (length defs))
               (funcall (second (car defs))))
              (t
               (let ((grep (make-grep "*Definitions*")))
                 (loop :for (file jump-fun) :in defs :do
                   (grep-append grep file jump-fun))
                 (grep-update grep))))))))

(define-key *lisp-mode-keymap* (kbd "M-,") 'lisp-pop-find-definition-stack)
(define-command lisp-pop-find-definition-stack () ()
  (let ((elt (pop *lisp-find-definition-stack*)))
    (when (null elt)
      (return-from lisp-pop-find-definition-stack nil))
    (destructuring-bind (buffer . point) elt
      (select-buffer buffer)
      (point-set point))))

(defun analyze-symbol (str)
  (let (package
        external-p)
    (let* ((list (uiop:split-string str :separator ":"))
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
  (let* ((end (current-point))
         (begin (prog2 (backward-sexp)
                    (current-point)
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
   (when (sexp-goto-car 100)
     (let* ((start (current-point))
            (end (progn (forward-sexp 1 t) (current-point))))
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
   (when (/= (current-linum)
             (save-excursion (skip-chars-forward '(#\space #\tab #\newline))
                             (current-linum)))
     (skip-chars-forward '(#\space #\tab #\newline)))
   (let ((start (region-beginning))
         (end (region-end)))
     (point-set end)
     (skip-chars-forward '(#\space #\tab))
     (unless (eolp)
       (insert-newline 1))
     (point-set start)
     (let ((charpos (point-charpos start)))
       (apply-region-lines start end
                           #'(lambda ()
                               (set-charpos charpos)
                               (unless (blank-line-p)
                                 (insert-string ";; "))))))))

(define-command lisp-uncomment-region () ()
  (let ((start (region-beginning))
        (end (region-end)))
    (point-set start)
    (do ()
        ((point<= end (current-point)))
      (skip-chars-forward '(#\space #\tab))
      (do ((delete-flag nil t))
          ((not (eql #\; (following-char)))
           (when (and delete-flag
                      (syntax-space-char-p (following-char)))
             (delete-char 1 nil)))
        (delete-char 1 nil))
      (forward-line 1))))

(defun lisp-print-values (values)
  (with-open-stream (out (make-buffer-output-stream))
    (dolist (v values)
      (pprint v out))
    (point-set (buffer-output-stream-point out))))

(define-key *lisp-mode-keymap* (kbd "C-x C-M-j") 'lisp-eval-print-last-sexp)
(define-command lisp-eval-print-last-sexp () ()
  (lisp-move-and-eval-sexp
   #'backward-sexp
   #'(lambda (string)
       (unless (bolp) (insert-newline))
       (let ((output-buffer (get-buffer-create "*output*")))
         (buffer-erase output-buffer)
         (setf (buffer-modified-p output-buffer) nil)
         (lisp-print-values
          (%lisp-eval-string string output-buffer nil nil
                             (lisp-current-package)))
         (insert-newline)
         (when (buffer-modified-p output-buffer)
           (lisp-info-popup output-buffer nil nil))))))

(define-major-mode lisp-repl-mode lisp-mode
  (:name "lisp-repl"
   :keymap *lisp-repl-mode-keymap*
   :syntax-table *lisp-syntax-table*)
  (setf (get-bvar :listener-get-prompt-function)
        'lisp-repl-get-prompt)
  (setf (get-bvar :listener-check-confirm-function)
        'lisp-repl-paren-correspond-p)
  (setf (get-bvar :listener-confirm-function)
        'lisp-repl-confirm)
  (listener-mode t))

(define-command start-lisp-repl () ()
  (listener-start "*lisp-repl*" 'lisp-repl-mode))

(defun shorten-package-name (package)
  (car
   (sort (copy-list
          (cons (package-name package)
                (package-nicknames package)))
         #'(lambda (x y)
             (< (length x) (length y))))))

(defun lisp-repl-get-prompt ()
  (shorten-package-name (lisp-current-package)))

(defun lisp-repl-paren-correspond-p ()
  (loop :with count := 0 :do
    (insert-string ")")
    (incf count)
    (unless (save-excursion (backward-sexp 1 t))
      (delete-char (- count) nil)
      (return (= 1 count)))))

(defun lisp-repl-confirm (string)
  (setq - (car (%string-to-exps string (lisp-current-package))))
  (multiple-value-bind (values error-p)
      (%lisp-eval - (current-buffer) (current-point) t)
    (declare (ignore error-p))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (end-of-buffer)
    (lisp-print-values values)
    (listener-reset-prompt)))

(define-key *lisp-repl-mode-keymap* (kbd "C-x p") 'lisp-repl-set-package)
(define-command lisp-repl-set-package () ()
  (lisp-set-package)
  (listener-reset-prompt)
  t)

(defun lisp-info-popup (buffer &optional output-function (focus-set-p t))
  (info-popup buffer output-function focus-set-p 'lisp-mode))

(setq *auto-mode-alist*
      (append '((".lisp$" . lisp-mode)
                (".asd$" . lisp-mode))
              *auto-mode-alist*))
