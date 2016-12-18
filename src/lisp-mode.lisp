(in-package :cl-user)
(defpackage :lem.lisp-mode
  (:use :cl :lem :lem.prog-mode :lem.listener-mode)
  (:import-from
   :lem.util)
  (:export
   :*indent-spec-function*
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
   :lisp-repl-set-package))
(in-package :lem.lisp-mode)

(defvar *indent-table* (make-hash-table :test 'equal))
(defvar *indent-spec-function* nil)

(loop :for (name . n)
      :in '(("block" . 1)
            ("cond" . progn)
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
            ("flet" . flet)
            ("labels" . flet)
            ("macrolet" . flet)
            ("generic-flet" . flet)
            ("generic-labels" . flet)
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
            ("progn" . progn)
            ("progv" . 2)
            ("return" . 0)
            ("return-from" . 1)
            ("symbol-macrolet" . 1)
            ("tagbody" . 0)
            ("throw" . 1)
            ("unless" . 1)
            ("unwind-protect" . 1)
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
            ("loop" . progn)
            (":method" . 1)
            (":use" . progn)
            (":export" . progn)
            (":import-from" . progn))
      :do (setf (gethash name *indent-table*) n))

(defun featurep (form positivep)
  (cond ((atom form)
         (if positivep
             (find (find-symbol (princ-to-string form)
                                :keyword)
                   *features*)
             (not (find (find-symbol (princ-to-string form)
                                     :keyword)
                        *features*))))
        ((eq 'and (car form))
         (every (lambda (form) (featurep form positivep))
                (cdr form)))
        ((eq 'or (car form))
         (every (lambda (form) (featurep form positivep))
                (cdr form)))
        (t)))

(defvar *feature-attribute* (lem::copy-attribute *syntax-comment-attribute*))

(defvar *lisp-syntax-table*
  (let ((table
         (make-syntax-table
          :space-chars '(#\space #\tab #\newline)
          :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\> #\= #\/ #\: #\. #\%)
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
          :block-comment-following-char #\|)))

    (syntax-add-match table
                      (make-syntax-test "(")
                      :matched-symbol :start-expr
                      :symbol-lifetime 1)

    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+" :regex-p t)
                      :test-symbol :define-start
                      :attribute *syntax-function-name-attribute*)

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
      (syntax-add-match table
                        (make-syntax-test str :word-p t)
                        :test-symbol :start-expr
                        :attribute *syntax-keyword-attribute*
                        :matched-symbol :define-start
                        :symbol-lifetime 1))

    (syntax-add-match table
                      (make-syntax-test "^(:?[^: \\t]+:)?define-[^ \\t()]*$"
                                        :regex-p t :word-p t)
                      :test-symbol :start-expr
                      :attribute *syntax-keyword-attribute*
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
      (syntax-add-match table
                        (make-syntax-test str :word-p t)
                        :test-symbol :start-expr
                        :attribute *syntax-keyword-attribute*))

    (syntax-add-match table
                      (make-syntax-test "^(?:[^:*]*:)?\\*[^*]+\\*$"
                                        :regex-p t :word-p t)
                      :attribute *syntax-variable-attribute*)

    (syntax-add-match table
                      (make-syntax-test "^:[^() \\t]+$"
                                        :regex-p t :word-p t)
                      :attribute *syntax-constant-attribute*)

    (syntax-add-match table
                      (make-syntax-test "^&[^() \\t]+$"
                                        :regex-p t :word-p t)
                      :attribute *syntax-constant-attribute*)

    (syntax-add-match table
                      (make-syntax-test "#[+-]" :regex-p t)
                      :move-action (lambda ()
                                     (ignore-errors
                                      (let ((positivep (eql #\+ (char-after 1))))
                                        (shift-position 2)
                                        (let ((prev-point (current-point)))
                                          (when (forward-sexp 1 t)
                                            (cond ((featurep (read-from-string
                                                              (region-string prev-point
                                                                             (current-point)))
                                                             positivep)
                                                   (setf (current-point) prev-point)
                                                   nil)
                                                  (t
                                                   (forward-sexp 1 t)
                                                   (current-point))))))))
                      :attribute *feature-attribute*)

    table))

(define-major-mode lisp-mode prog-mode
    (:name "lisp"
     :keymap *lisp-mode-keymap*
     :syntax-table *lisp-syntax-table*)
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :indent-tabs-mode) nil)
  (modeline-add-status-list (lambda (window)
                              (package-name (lisp-current-package
                                             (window-buffer window))))
                            (current-buffer))
  (setf (get-bvar :calc-indent-function)
        'lisp-calc-indent)
  (setf (get-bvar :beginning-of-defun-function)
        'lisp-beginning-of-defun)
  (setf (get-bvar :end-of-defun-function)
        'lisp-end-of-defun))

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
             ;;    (shift-position 2))
             ;;  (if (eql #\( (following-char))
             ;;      (skip-list-forward 0)
             ;;      (skip-symbol-forward))
             ;;  (skip-chars-forward '(#\space #\tab #\newline)))
             (t
              (shift-position 2))))))

(defun lisp-mode-skip-expr-prefix-backward ()
  (%lisp-mode-skip-expr-prefix (char-before 2)
                               (char-before 1)
                               #'(lambda (c1 c2)
                                   (declare (ignore c1 c2))
                                   (shift-position -2))))

(defun looking-at-indent-spec ()
  (let* ((string (symbol-string-at-point))
         (pos (and string (position #\: string :from-end t)))
         (name (if (and pos (plusp pos))
                   (subseq string (1+ pos))
                   string))
         result)
    (when (and *indent-spec-function*
               (setf result (funcall *indent-spec-function* string)))
      (return-from looking-at-indent-spec result))
    (when (and name (setf result (gethash name *indent-table*)))
      (return-from looking-at-indent-spec result))
    (let ((arglist (lisp-search-arglist string #'lisp-get-arglist)))
      (when (and arglist (setf result (position '&body arglist)))
        (return-from looking-at-indent-spec result)))
    (when (ppcre:scan "^(?:def|with-|do-)" name)
      0)))

(defun go-to-car ()
  (loop :while (backward-sexp 1 t) :count t))

(defun flet-indent-p ()
  (save-excursion
   (when (up-list 1 t)
     (go-to-car)
     (and (eql #\( (following-char))
          (up-list 1 t)
          (progn
            (go-to-car)
            (eq 'flet (looking-at-indent-spec)))))))

(defun arg1-first-line-p ()
  (when (forward-sexp 1 t)
    (skip-chars-forward '(#\space #\tab))
    (not (eolp))))

(defun calc-indent-1 ()
  (let* ((arg-count (go-to-car))
         (spec (looking-at-indent-spec))
         (car-column (1- (current-column))))
    (cond ((integerp spec)
           (+ car-column
              (if (<= arg-count spec)
                  4
                  2)))
          ((eq spec 'flet)
           (+ car-column 2))
          ((eq spec 'progn)
           (if (arg1-first-line-p)
               (current-column)
               (+ car-column 2)))
          ((flet-indent-p)
           (+ car-column 2))
          ((eql #\: (following-char))
           (1+ car-column))
          (t
           (if (arg1-first-line-p)
               (current-column)
               (1+ car-column))))))

(defun lisp-calc-indent ()
  (save-excursion
    (beginning-of-line)
    (when (eq *syntax-string-attribute*
              (text-property-at (current-marker) :attribute -1))
      (return-from lisp-calc-indent nil))
    (when (save-excursion (and (backward-sexp 1 t) (bolp)))
      (return-from lisp-calc-indent 0))
    (calc-indent-1)))

(define-key *lisp-mode-keymap* (kbd "C-M-q") 'lisp-indent-sexp)
(define-command lisp-indent-sexp () ()
  (indent-region (current-point)
                 (save-excursion
                  (forward-sexp 1)
                  (current-point))))

(define-command lisp-beginning-of-defun (&optional (n 1)) ("p")
  (beginning-of-defun-abstract n #'(lambda () (looking-at-line "^\\("))))

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
                    (return (forward-line 1)))
                   ((syntax-space-char-p c)
                    (unless (shift-position 1)
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
               (remove "package" (get-bvar :file-property-list)
                       :test #'equal
                       :key #'car))))

(defun lisp-read-change-package (find-package-function
                                 complete-package-function)
  (let* ((package-name
          (string-upcase
           (minibuf-read-line "Package: " ""
                              complete-package-function nil
                              'mh-lisp-package)))
         (package (funcall find-package-function package-name)))
    (cond (package
           (lisp-change-package package) t)
          (t
           (message "Package does not exist: ~a" package-name)
           nil))))

(define-key *lisp-mode-keymap* (kbd "C-c M-p") 'lisp-set-package)
(define-command lisp-set-package () ()
  (lisp-read-change-package
   #'find-package
   #'(lambda (str)
       (completion str
                   (mapcar #'(lambda (pkg)
                               (string-downcase (package-name pkg)))
                           (list-all-packages))))))

(defun scan-current-package (check-package-fn)
  (save-excursion
    (loop (multiple-value-bind (result groups)
              (looking-at-line "\\(in-package (?:#?:|')([^\)]*)\\)")
            (when result
              (let ((package (funcall check-package-fn (aref groups 0))))
                (when package
                  (return package))))
            (unless (forward-line -1)
              (return))))))

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
             (message "~a" cdt)))
    (unless error-p
      (store-value x condition)
      (return))))

(defvar *error-buffer-name* "*error*")

(defun lisp-debugger (condition)
  (let* ((choices (compute-restarts condition))
         (n (length choices)))
    (with-pop-up-typeout-window (out (get-buffer-create *error-buffer-name*) :erase t)
      (format out "~a~%~%" condition)
      (loop
        for choice in choices
        for i from 1
        do (format out "~&[~d] ~a~%" i choice))
      (terpri out)
      (uiop/image:print-backtrace :stream out :count 100))
    (loop
      (redraw-display)
      (handler-case
          (let* ((str (minibuf-read-string "Debug: "))
                 (i (and (stringp str) (parse-integer str :junk-allowed t))))
            (cond ((and i (<= 1 i n))
                   (let ((restart (nth (1- i) choices)))
                     (cond ((eq 'store-value (restart-name restart))
                            (ldebug-store-value condition))
                           (t
                            (invoke-restart-interactively restart))))
                   (return))
                  (t
                   (let ((x
                          (handler-case (eval (read-from-string str nil))
                            (error (cdt) (format nil "~a" cdt)))))
                     (with-pop-up-typeout-window (out (get-buffer-create "*output*") :erase t)
                       (princ x out))))))
        (lem::editor-abort ()))))
  condition)

(defun %lisp-eval-internal (x output-buffer point &optional update-point-p)
  (with-open-stream (io (make-editor-io-stream output-buffer point t))
    (let* ((error-p)
           (results)
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
                                  (lem::editor-abort () :report "Abort.")))
                          (when update-point-p
                            (point-set
                             (buffer-output-stream-point io))))
                         ((condition)
                          (setq error-p t)
                          (setq results (list condition))))
      (values results error-p))))

(defun %lisp-eval (x output-buffer point
                     &optional update-point-p)
  (unless point (setq point (point-min)))
  (lem::call-with-allow-interrupt
   t
   (lambda ()
     (multiple-value-bind (results error-p)
         (%lisp-eval-internal x
                              output-buffer
                              point
                              update-point-p)
       (values results error-p)))))

(defun %lisp-eval-string (string output-buffer point
                                 &optional
                                 update-point-p (package "COMMON-LISP-USER"))
  (%lisp-eval `(cl:progn ,@(%string-to-exps string package))
              output-buffer
              point
              update-point-p))

(define-key *global-keymap* (kbd "M-:") 'lisp-eval-string)
(define-command lisp-eval-string (string)
    ((list (minibuf-read-line "Eval: " "" nil nil 'mh-lisp-eval)))
  (let ((output-buffer (get-buffer-create "*output*")))
    (buffer-erase output-buffer)
    (change-buffer-mode output-buffer 'lisp-mode)
    (buffer-unmark output-buffer)
    (message "~{~s~^,~}"
             (%lisp-eval-string string output-buffer nil nil
                                (lisp-current-package)))
    (when (buffer-modified-p output-buffer)
      (pop-up-typeout-window output-buffer nil))))

(define-key *lisp-mode-keymap* (kbd "C-c C-r") 'lisp-eval-region)
(define-command lisp-eval-region (&optional begin end) ("r")
  (unless (or begin end)
    (setq begin (region-beginning))
    (setq end (region-end)))
  (lisp-eval-string (region-string begin end))
  t)

(defun lisp-move-and-eval-sexp (move-sexp eval-string-function)
  (let ((str (save-excursion
              (and (funcall move-sexp)
                   (region-string (current-point)
                                  (progn
                                    (forward-sexp 1)
                                    (current-point)))))))
    (when str
      (funcall eval-string-function str)
      t)))

(define-key *lisp-mode-keymap* (kbd "C-M-x") 'lisp-eval-defun)
(define-command lisp-eval-defun () ()
  (lisp-move-and-eval-sexp #'top-of-defun #'lisp-eval-string))

(define-key *lisp-mode-keymap* (kbd "C-c C-e") 'lisp-eval-last-sexp)
(define-command lisp-eval-last-sexp () ()
  (lisp-move-and-eval-sexp #'backward-sexp #'lisp-eval-string))

(define-key *lisp-mode-keymap* (kbd "C-c l") 'lisp-load-file)
(define-key *lisp-mode-keymap* (kbd "C-c C-l") 'lisp-load-file)
(define-command lisp-load-file (filename) ("fLoad File: ")
  (when (uiop:pathname-equal filename (buffer-directory))
    (setf filename (buffer-filename (current-buffer))))
  (when (and (cl-fad:file-exists-p filename)
             (not (cl-fad:directory-pathname-p filename)))
    (lisp-eval-string
     (format nil "(cl:load ~s)" filename))
    (message "load: ~A" filename)))

(defun lisp-print-error (condition)
  (with-pop-up-typeout-window (out (get-buffer-create *error-buffer-name*) :erase t)
    (format out "~a~%~%" condition)
    (uiop/image:print-backtrace :stream out :count 100)))

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
  (delete-region (current-point)
                 (progn
                   (forward-sexp 1)
                   (current-point)))
  (with-open-stream (stream (make-buffer-output-stream (current-buffer) (current-point)))
    (pprint expr stream))
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
      (with-pop-up-typeout-window (out (change-buffer-mode (get-buffer-create buffer-name)
                                                           'lisp-mode)
                                       :focus nil
                                       :erase t)
        (pprint expr out)))))

(define-key *lisp-mode-keymap* (kbd "C-c m") 'lisp-macroexpand)
(define-key *lisp-mode-keymap* (kbd "C-c C-m") 'lisp-macroexpand)
(define-command lisp-macroexpand () ()
  (%lisp-macroexpand 'macroexpand-1 "*macroexpand*"))

(define-key *lisp-mode-keymap* (kbd "C-c M") 'lisp-macroexpand-all)
(define-key *lisp-mode-keymap* (kbd "C-c M-m") 'lisp-macroexpand-all)
(define-command lisp-macroexpand-all () ()
  (%lisp-macroexpand 'swank/backend:macroexpand-all
                     "*macroexpand*"))

(defun lisp-read-symbol (prompt history-name)
  (let ((default-name (or (symbol-string-at-point) "")))
    (let ((name (minibuf-read-line prompt
                                   default-name
                                   'complete-symbol
                                   nil
                                   history-name)))
      (setq name (string-right-trim ":" name))
      (with-safe-form
        (let ((*package* (lisp-current-package)))
          (read-from-string name))))))

(define-key *lisp-mode-keymap* (kbd "C-c C-d") 'lisp-describe-symbol)
(define-command lisp-describe-symbol () ()
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Describe: " 'mh-describe)
    (unless error-p
      (with-pop-up-typeout-window (out (change-buffer-mode (get-buffer-create "*describe*")
                                                           'lisp-mode)
                                       :erase t)
        (describe name out)))))

(define-key *lisp-mode-keymap* (kbd "C-c M-d") 'lisp-disassemble-symbol)
(define-command lisp-disassemble-symbol () ()
  (multiple-value-bind (name error-p)
      (lisp-read-symbol "Disassemble: " 'mh-disassemble)
    (unless error-p
      (let ((str
             (with-output-to-string (out)
               (handler-case (disassemble name :stream out)
                 (error (condition)
                        (message "~a" condition)
                        (return-from lisp-disassemble-symbol nil))))))
        (with-pop-up-typeout-window (out (change-buffer-mode (get-buffer-create "*disassemble*")
                                                             'lisp-mode))
          (princ str out))))))

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
      (lisp-read-symbol "Find definitions: " 'mh-find-definitions)
    (unless error-p
      (let ((defs))
        (dolist (def (lisp-find-definitions-internal name))
          (destructuring-bind (head file filepos) def
            (declare (ignore head))
            (push (list file
                        (lambda ()
                          (find-file file)
                          (goto-position filepos)
                          (redraw-display)))
                  defs)))
        (push (cons (current-buffer) (current-point))
              *lisp-find-definition-stack*)
        (cond ((= 1 (length defs))
               (funcall (second (car defs))))
              (t
               (lem.sourcelist:with-sourcelist (sourcelist "*Definitions*")
                 (loop :for (file jump-fun) :in defs
                       :do (lem.sourcelist:append-sourcelist
                            sourcelist
                            (lambda (cur-marker)
                              (lem::insert-string-at cur-marker file))
                            jump-fun)))))))))

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
          (completion-hypheen str
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

(define-key *lisp-mode-keymap* (kbd "C-M-i") 'lisp-complete-symbol)
(define-command lisp-complete-symbol () ()
  (start-completion #'complete-symbol
                    (lisp-preceding-symbol))
  t)

(defun lisp-get-arglist (symbol)
  (when (fboundp symbol)
    (values (swank/backend:arglist symbol) t)))

(defun lisp-get-arglist-string (symbol)
  (multiple-value-bind (arglist foundp)
      (lisp-get-arglist symbol)
    (when foundp
      (if (null arglist)
          "()"
          (ppcre:regex-replace-all "\\s+" (princ-to-string arglist) " ")))))

(defun lisp-search-arglist (string arglist-fn)
  (multiple-value-bind (x error-p)
      (ignore-errors
       (destructuring-bind (package symbol-name external-p)
           (analyze-symbol string)
         (declare (ignore external-p))
         (multiple-value-bind (symbol status)
             (find-symbol (string-readcase symbol-name)
                          (or package
                              (lisp-current-package)))
           (if (null status)
               nil
               symbol))))
    (when (and (not error-p)
               (symbolp x))
      (funcall arglist-fn x))))

(defun search-backward-arglist ()
  (save-excursion
   (loop
     (go-to-car)
     (let ((name (symbol-string-at-point)))
       (multiple-value-bind (arglist)
           (lisp-search-arglist name #'lisp-get-arglist-string)
         (cond (arglist
                (return (values (format nil "~A: ~A" name arglist) t)))
               ((not (up-list 1 t))
                (return nil))))))))

(define-key *lisp-mode-keymap* (kbd "C-c M-h") 'lisp-echo-arglist)
(define-command lisp-echo-arglist () ()
  (multiple-value-bind (arglist foundp)
      (search-backward-arglist)
    (when foundp
      (message "~A" arglist))))

(define-key *lisp-mode-keymap* (kbd "C-c ;") 'lisp-comment-or-uncomment-region)
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
     (point-set start)
     (let ((charpos (point-charpos start)))
       (apply-region-lines start end
                           #'(lambda ()
                               (end-of-line)
                               (if (< charpos (current-charpos))
                                   (set-charpos charpos)
                                   (beginning-of-line))
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

(defun check-package (package-name)
  (find-package (string-upcase package-name)))

(let ((prev-point nil))
  (defun lisp-idle-timer-function ()
    (when (eq (major-mode) 'lisp-mode)
      (let ((package (scan-current-package #'check-package)))
        (when package
          (lisp-change-package package))))
    (let ((curr-point (current-point)))
      (when (or (null prev-point)
                (not (point= prev-point curr-point)))
        (when (member (major-mode) '(lisp-mode lisp-repl-mode))
          (setf prev-point curr-point)
          (lisp-echo-arglist)
          (redraw-display))))))

(defvar *lisp-timer*)
(when (or (not (boundp '*lisp-timer*))
          (not (timer-alive-p *lisp-timer*)))
  (setf *lisp-timer*
        (start-idle-timer "lisp" 500 t 'lisp-idle-timer-function nil
                          (lambda (condition)
                            (pop-up-backtrace condition)
                            (stop-timer *lisp-timer*)))))

(defun lisp-print-values (values)
  (with-open-stream (out (make-buffer-output-stream (current-buffer) (current-point)))
    (let ((*package* (lisp-current-package)))
      (dolist (v values)
        (pprint v out)))
    (point-set (buffer-output-stream-point out))))

(define-key *lisp-mode-keymap* (kbd "C-c C-j") 'lisp-eval-print-last-sexp)
(define-command lisp-eval-print-last-sexp () ()
  (lisp-move-and-eval-sexp
   #'backward-sexp
   #'(lambda (string)
       (unless (bolp) (insert-newline))
       (setq - (first (%string-to-exps string (lisp-current-package))))
       (let ((values (%lisp-eval - (current-buffer) (current-point) t)))
         (setq +++ ++ /// //     *** (car ///)
               ++  +  //  /      **  (car //)
               +   -  /   values *   (car /))
         (lisp-print-values values)
         (insert-newline)))))

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
  (format nil "~A> " (shorten-package-name (lisp-current-package))))

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
    (point-set (point-max))
    (lisp-print-values values)
    (listener-reset-prompt)))

(define-key *lisp-repl-mode-keymap* (kbd "C-c M-p") 'lisp-repl-set-package)
(define-command lisp-repl-set-package () ()
  (lisp-set-package)
  (listener-reset-prompt)
  t)

(pushnew (cons ".lisp$" 'lisp-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons ".asd$" 'lisp-mode) *auto-mode-alist* :test #'equal)
