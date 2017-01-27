(in-package :cl-user)
(defpackage :lem.lisp-mode
  (:use :cl :lem :lem.listener-mode)
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

(defun featurep (form)
  (cond ((atom form)
         (find (find-symbol (princ-to-string form)
                            :keyword)
               *features*))
        ((string-equal 'and (car form))
         (every #'featurep (cdr form)))
        ((string-equal 'or (car form))
         (some #'featurep (cdr form)))
        (t)))

(defvar *feature-attribute* (copy-attribute *syntax-comment-attribute*))

(defparameter +symbol-package-prefix+
  '(:sequence
    (:greedy-repetition 1 nil (:inverted-char-class #\( #\) #\space #\tab)) #\:))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defvar *lisp-syntax-table*
  (let ((table
         (make-syntax-table
          :space-chars '(#\space #\tab #\newline)
          :symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@ #\[ #\] #\^ #\{ #\} #\~ #\# #\|)
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
                      (make-syntax-test ":[^() \\t]+"
                                        :word-p t)
                      :attribute *syntax-constant-attribute*)
    (syntax-add-match table
                      (make-syntax-test "\\(")
                      :matched-symbol :start-form
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :define-start
                      :attribute *syntax-function-name-attribute*)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :defpackage-start
                      :attribute *syntax-type-attribute*)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :defvar-start
                      :attribute *syntax-variable-attribute*)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "defun" "defclass" "defgeneric" "defsetf" "defmacro" "defmethod")
                          (:sequence "define-" (:greedy-repetition 0 nil
                                                (:inverted-char-class #\space #\tab #\( #\))))))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :define-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "deftype" "defpackage" "defstruct")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :defpackage-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort "defvar" "defparameter" "defconstant")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :defvar-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "block" "case" "ccase" "ecase" "typecase" "etypecase" "ctypecase" "catch"
                             "cond" "destructuring-bind" "do" "do*" "dolist" "dotimes"
                             "eval-when" "flet" "labels" "macrolet" "generic-flet" "generic-labels"
                             "handler-case" "restart-case" "if" "lambda" "let" "let*" "handler-bind"
                             "restart-bind" "locally" "multiple-value-bind" "multiple-value-call"
                             "multiple-value-prog1" "prog" "prog*" "prog1" "prog2" "progn" "progv" "return"
                             "return-from" "symbol-macrolet" "tagbody" "throw" "unless" "unwind-protect"
                             "when" "with-accessors" "with-condition-restarts" "with-open-file"
                             "with-output-to-string" "with-slots" "with-standard-io-syntax" "loop"
                             "declare" "declaim" "proclaim")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*)
    (syntax-add-match table
                      (make-syntax-test "&[^() \\t]+"
                                        :word-p t)
                      :attribute *syntax-constant-attribute*)
    (syntax-add-match
     table
     (make-syntax-test "#[+-]")
     :move-action (lambda (cur-point)
                    (ignore-errors
                     (let ((positivep (eql #\+ (character-at cur-point 1))))
                       (character-offset cur-point 2)
                       (with-point ((prev cur-point))
                         (when (form-offset cur-point 1)
                           (cond
                             ((if (featurep (read-from-string
                                             (points-to-string
                                              prev cur-point)))
                                  positivep
                                  (not positivep))
                              nil)
                             (t
                              (form-offset cur-point 1))))))))
     :attribute *feature-attribute*)
    table))

(define-major-mode lisp-mode nil
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

(defun lisp-mode-skip-expr-prefix-forward (point)
  (%lisp-mode-skip-expr-prefix
   (character-at point 0)
   (character-at point 1)
   (lambda (c1 c2)
     (declare (ignore c1 c2))
     (character-offset point 2))))

(defun lisp-mode-skip-expr-prefix-backward (point)
  (%lisp-mode-skip-expr-prefix
   (character-at point -2)
   (character-at point -1)
   (lambda (c1 c2)
     (declare (ignore c1 c2))
     (character-offset point -2))))

(defun looking-at-indent-spec ()
  (let* ((string (symbol-string-at-point (current-point)))
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
  (let ((point (current-point)))
    (when (form-offset point 1)
      (skip-chars-forward point '(#\space #\tab))
      (not (end-line-p point)))))

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

(defun lisp-calc-indent (point)
  (save-excursion
    (setf (current-buffer) (point-buffer point))
    (move-point (current-point) point)
    (beginning-of-line)
    (when (eq *syntax-string-attribute*
              (text-property-at (current-point) :attribute -1))
      (return-from lisp-calc-indent nil))
    (when (save-excursion (and (backward-sexp 1 t) (bolp)))
      (return-from lisp-calc-indent 0))
    (calc-indent-1)))

(define-key *lisp-mode-keymap* (kbd "C-M-q") 'lisp-indent-sexp)
(define-command lisp-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-region (current-point) end))))

(defun top-of-defun (point)
  (loop :while (scan-lists point -1 1 t))
  t)

(defun beginning-of-defun (point n)
  (with-point ((start point))
    (if (minusp n)
        (dotimes (_ (- n) point)
          (if (start-line-p point)
              (line-offset point -1)
              (line-start point))
          (loop
            (when (char= #\( (character-at point 0))
              (return))
            (unless (line-offset point -1)
              (move-point point start)
              (return-from beginning-of-defun nil))))
        (dotimes (_ n point)
          (loop
            (unless (line-offset point 1)
              (move-point point start)
              (return-from beginning-of-defun nil))
            (when (char= #\( (character-at point 0))
              (return)))))))

(define-key *lisp-mode-keymap* (kbd "C-M-a") 'lisp-beginning-of-defun)
(define-command lisp-beginning-of-defun (n) ("p")
  (beginning-of-defun (current-point) (- n)))

(define-key *lisp-mode-keymap* (kbd "C-M-e") 'lisp-end-of-defun)
(define-command lisp-end-of-defun (n) ("p")
  (if (minusp n)
      (lisp-beginning-of-defun (- n))
      (let ((point (current-point)))
        (dotimes (_ n)
          (skip-chars-forward point '(#\space #\tab))
          (with-point ((prev point))
            (beginning-of-defun point -1)
            (form-offset point 1)
            (if (point< point prev)
                (if (beginning-of-defun prev 1)
                    (move-point point prev)
                    (progn
                      (move-point point prev)
                      (return-from lisp-end-of-defun nil)))
                point))
          (skip-chars-forward point '(#\space #\tab))
          (when (char= #\newline (character-at point))
            (character-offset point 1))))))

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
           (prompt-for-line "Package: " ""
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
              (looking-at-line "\\(in-package (?:#?:|')?([^\)]*)\\)")
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
                (prompt-for-string
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
	   (let* ((str (prompt-for-string "Debug: "))
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
	 (editor-abort ()))))
  condition)

(defun %lisp-eval-internal (x point &optional update-point-p)
  (with-point ((cur-point point
			  (if update-point-p
			      :left-inserting
			      :right-inserting)))
    (with-open-stream (io (make-editor-io-stream cur-point t))
      (let* ((error-p)
             (results)
             (*terminal-io* io)
             (*standard-output* io)
             (*standard-input* io)
             (*error-output* io)
             (*query-io* io)
             (*debug-io* io)
             (*trace-output* io)
             (*package* (lisp-current-package)))
        (handler-case-bind (#'lisp-debugger
                            (setq results
                                  (restart-case
                                      (multiple-value-list (eval x))
                                    (editor-abort () :report "Abort.")))
                            (when update-point-p
                              (move-point point cur-point)))
                           ((condition)
                            (setq error-p t)
                            (setq results (list condition))))
        (values results error-p)))))

(defun %lisp-eval (x point
		   &optional update-point-p)
  (multiple-value-bind (results error-p)
      (%lisp-eval-internal x
                           point
                           update-point-p)
    (values results error-p)))

(defun %lisp-eval-string (string point
			  &optional
			    update-point-p (package "COMMON-LISP-USER"))
  (%lisp-eval `(cl:progn ,@(%string-to-exps string package))
              point
              update-point-p))

(define-key *global-keymap* (kbd "M-:") 'lisp-eval-string)
(define-command lisp-eval-string (string)
    ((list (prompt-for-line "Eval: " "" nil nil 'mh-lisp-eval)))
  (let ((output-buffer (get-buffer-create "*output*")))
    (erase-buffer output-buffer)
    (change-buffer-mode output-buffer 'lisp-mode)
    (buffer-unmark output-buffer)
    (message "~{~s~^,~}"
             (%lisp-eval-string string (buffers-start output-buffer)
                                nil
                                (lisp-current-package)))
    (when (buffer-modified-p output-buffer)
      (pop-up-typeout-window output-buffer nil))))

(define-key *lisp-mode-keymap* (kbd "C-c C-r") 'lisp-eval-region)
(define-command lisp-eval-region (&optional start end) ("r")
  (unless (or start end)
    (setq start (region-beginning))
    (setq end (region-end)))
  (lisp-eval-string (points-to-string start end))
  t)

(defun lisp-move-and-eval-sexp (move-sexp eval-string-function)
  (let ((str (save-excursion
               (and (funcall move-sexp)
                    (let ((end (form-offset (copy-point (current-point) :temporary) 1)))
                      (when end
                        (points-to-string (current-point) end)))))))
    (when str
      (funcall eval-string-function str)
      t)))

(define-key *lisp-mode-keymap* (kbd "C-M-x") 'lisp-eval-defun)
(define-command lisp-eval-defun () ()
  (lisp-move-and-eval-sexp (lambda () (top-of-defun (current-point)))
                           #'lisp-eval-string))

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
             (points-to-string
              (current-point)
              (form-offset (copy-point (current-point)
				       :temporary)
			   1)))))
        (lisp-current-package))))

(defun %lisp-macroexpand-replace-expr (expr)
  (delete-between-points
   (current-point)
   (form-offset (copy-point (current-point) :temporary) 1))
  (with-open-stream (stream (make-buffer-output-stream (current-point)))
    (pprint expr stream))
  (let ((*package* (lisp-current-package)))
    (read-from-string
     (points-to-string (buffers-start (current-buffer))
                       (buffers-end (current-buffer))))))

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

(defun lisp-read-symbol (prompt history-name &optional use-default-name)
  (let ((default-name (symbol-string-at-point (current-point))))
    (let ((name (if (and use-default-name default-name)
                    default-name
                    (prompt-for-line prompt
                                     (or default-name "")
                                     'complete-symbol
                                     nil
                                     history-name))))
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
      (lisp-read-symbol "Find definitions: " 'mh-find-definitions t)
    (unless error-p
      (let ((defs))
        (dolist (def (lisp-find-definitions-internal name))
          (destructuring-bind (head file filepos) def
            (declare (ignore head))
            (push (list file
                        (lambda ()
                          (find-file file)
                          (move-to-position (current-point) filepos)
                          (redraw-display)))
                  defs)))
        (push (list (buffer-name (current-buffer))
                    (position-at-point (current-point)))
              *lisp-find-definition-stack*)
        (cond ((= 1 (length defs))
               (funcall (second (car defs))))
              (t
               (lem.sourcelist:with-sourcelist (sourcelist "*Definitions*")
                 (loop :for (file jump-fun) :in defs
		    :do (lem.sourcelist:append-sourcelist
			 sourcelist
			 (lambda (cur-point)
			   (insert-string cur-point file))
			 jump-fun)))))))))

(define-key *lisp-mode-keymap* (kbd "M-,") 'lisp-pop-find-definition-stack)
(define-command lisp-pop-find-definition-stack () ()
  (let ((elt (pop *lisp-find-definition-stack*)))
    (unless elt
      (return-from lisp-pop-find-definition-stack nil))
    (destructuring-bind (buffer-name offset) elt
      (let ((buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (move-to-position (current-point) offset)))))

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

(defvar *fuzzy-completions* nil)

(define-key *lisp-mode-keymap* (kbd "C-M-i") 'lisp-complete-symbol)
(define-command lisp-complete-symbol () ()
  (unless *fuzzy-completions*
    (swank:swank-require "SWANK-FUZZY")
    (setf *fuzzy-completions* (intern "FUZZY-COMPLETIONS" :swank)))
  (with-point ((start (current-point))
               (end (current-point)))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (let* ((result (let ((swank::*buffer-package* (lisp-current-package))
                         (swank::*buffer-readtable* *readtable*))
                     (funcall *fuzzy-completions*
                              (points-to-string start end)
                              (lisp-current-package))))
           (completions (car result)))
      (when completions
        (run-completion
         (mapcar (lambda (completion)
                   (lem::make-completion-item :label (first completion)
                                              :detail (fourth completion)
                                              :start start
                                              :end end))
                 completions))))))

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
       (let ((name (symbol-string-at-point (current-point))))
	 (multiple-value-bind (arglist)
	     (lisp-search-arglist name #'lisp-get-arglist-string)
	   (cond (arglist
		  (return (values (format nil "~A: ~A" name arglist) t)))
		 ((not (up-list 1 t))
		  (return nil))))))))

(define-key *lisp-mode-keymap* (kbd "C-c C-a") 'lisp-echo-arglist)
(define-command lisp-echo-arglist () ()
  (multiple-value-bind (arglist foundp)
      (search-backward-arglist)
    (when foundp
      (message "~A" arglist))))

(define-key *lisp-mode-keymap* (kbd "Spc") 'lisp-insert-space-and-echo-arglist)
(define-command lisp-insert-space-and-echo-arglist (n) ("p")
  (insert-character (current-point) #\space n)
  (lisp-echo-arglist))

(define-key *lisp-mode-keymap* (kbd "C-c ;") 'lisp-comment-or-uncomment-region)
(define-command lisp-comment-or-uncomment-region (arg) ("P")
  (if arg
      (lisp-uncomment-region)
      (lisp-comment-region)))

(defun space*-p (point)
  (with-point ((point point))
    (skip-chars-forward point '(#\space #\tab))
    (end-line-p point)))

(define-command lisp-comment-region () ()
  (save-excursion
    (with-point ((start (region-beginning) :right-inserting)
                 (end (region-end) :left-inserting))
      (let ((charpos (point-charpos start)))
        (loop
          (when (same-line-p start end)
            (cond ((space*-p start))
                  (t
                   (insert-string start ";; ")
                   (unless (space*-p end)
                     (insert-character end #\newline))))
            (return))
          (unless (space*-p start)
            (insert-string start ";; "))
          (line-offset start 1 charpos))))))

(define-command lisp-uncomment-region () ()
  (when (buffer-mark-p (current-buffer))
    (with-point ((start (region-beginning) :right-inserting)
                 (end (region-end) :right-inserting))
      (character-offset start -1)
      (loop
        ;; ここを実行中は構文走査がされないのでテキストプロパティが更新されず、ずれていくので後ろから探していく
        (unless (previous-single-property-change end :attribute start)
          (return))
        (when (and (eq *syntax-comment-attribute* (text-property-at end :attribute 0))
                   (not (eq *syntax-comment-attribute* (text-property-at end :attribute -1))))
          (if (looking-at end ";; ")
              (delete-character end 3)
              (loop :while (char= #\; (character-at end 0))
                    :do (delete-character end 1))))))))

(defun check-package (package-name)
  (find-package (string-upcase package-name)))

(defun lisp-idle-timer-function ()
  (when (eq (buffer-major-mode (current-buffer)) 'lisp-mode)
    (let ((package (scan-current-package #'check-package)))
      (when package
        (lisp-change-package package)))))

(defvar *lisp-timer*)
(when (or (not (boundp '*lisp-timer*))
          (not (timer-alive-p *lisp-timer*)))
  (setf *lisp-timer*
        (start-idle-timer "lisp" 110 t 'lisp-idle-timer-function nil
                          (lambda (condition)
                            (pop-up-backtrace condition)
                            (stop-timer *lisp-timer*)))))

(defun lisp-print-values (point values)
  (with-point ((point point :left-inserting))
    (with-open-stream (out (make-buffer-output-stream point))
      (let ((*package* (lisp-current-package)))
        (dolist (v values)
          (pprint v out))))))

(define-key *lisp-mode-keymap* (kbd "C-c C-j") 'lisp-eval-print-last-sexp)
(define-command lisp-eval-print-last-sexp () ()
  (lisp-move-and-eval-sexp
   #'backward-sexp
   #'(lambda (string)
       (unless (bolp) (insert-character (current-point) #\newline))
       (setq - (first (%string-to-exps string (lisp-current-package))))
       (let ((point (current-point)))
         (let ((values (%lisp-eval - point t)))
           (setq +++ ++ /// //     *** (car ///)
                 ++  +  //  /      **  (car //)
                 +   -  /   values *   (car /))
           (lisp-print-values point values)
           (insert-character point #\newline))))))

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

(defun lisp-repl-paren-correspond-p (point)
  (loop :with count := 0 :do
     (insert-character point #\))
     (incf count)
     (unless (form-offset (copy-point point :temporary) -1)
       (delete-character point (- count))
       (return (= 1 count)))))

(defun lisp-repl-confirm (point string)
  (setq - (car (%string-to-exps string (lisp-current-package))))
  (multiple-value-bind (values error-p)
      (%lisp-eval - point t)
    (declare (ignore error-p))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (buffer-end point)
    (lisp-print-values point values)
    (listener-reset-prompt (point-buffer point))))

(define-key *lisp-repl-mode-keymap* (kbd "C-c M-p") 'lisp-repl-set-package)
(define-command lisp-repl-set-package () ()
  (lisp-set-package)
  (listener-reset-prompt)
  t)

(pushnew (cons ".lisp$" 'lisp-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons ".asd$" 'lisp-mode) *auto-mode-alist* :test #'equal)
