(in-package :cl-user)
(defpackage :lem-go-mode
  (:use :cl :lem :lem.language-mode)
  (:import-from
   :lem.tmlanguage
   :load-tmlanguage)
  (:export))
(in-package :lem-go-mode)

(defvar *go-builtin*
  '("append" "cap"   "close"   "complex" "copy"
    "delete" "imag"  "len"     "make"    "new"
    "panic"  "print" "println" "real"    "recover"))

(defvar *go-keywords*
  '("break"    "default"     "func"   "interface" "select"
    "case"     "defer"       "go"     "map"       "struct"
    "chan"     "else"        "goto"   "package"   "switch"
    "const"    "fallthrough" "if"     "range"     "type"
    "continue" "for"         "import" "return"    "var"))

(defvar *go-constants*
  '("nil" "true" "false" "iota"))

(defvar *go-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-alist '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (load-tmlanguage
                     (merge-pathnames "go.json"
                                      (merge-pathnames "lem-go-mode/"
                                                       (asdf:system-source-directory :lem))))))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode go-mode language-mode
    (:name "go"
     :keymap *go-mode-keymap*
     :syntax-table *go-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'go-calc-indent)
  (setf (variable-value 'indent-tabs-mode) t)
  (setf (variable-value 'tab-width) 8)
  (setf (variable-value 'beginning-of-defun-function) 'go-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'go-end-of-defun)
  (setf (variable-value 'line-comment) "//")
  (setf (variable-value 'insertion-line-comment) "// ")
  (setf (variable-value 'find-definitions-function) 'find-definitions)
  (setf (variable-value 'completion-function) 'go-completion))

(defun go-beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w[^=(]*")))

(defun go-end-of-defun (point n)
  (if (minusp n)
      (go-beginning-of-defun point (- n))
      (search-forward-regexp point "^(?:\\}|\\))")))

(defun following-word (point)
  (points-to-string point
		    (form-offset (copy-point point :temporary) 1)))

(defun semicolon-p (point)
  (let ((c (character-at point -1)))
    (case c
      ((#\;) t)
      ((#\' #\" #\`) t)
      ((#\+ #\-)
       (character-offset point -1)
       (eql c (character-at point -1)))
      ((#\) #\] #\}) t)
      (t
       (and (/= 0 (skip-chars-backward point #'syntax-word-char-p))
            #-(and)(not (member (following-word point)
                                '("break" "continue" "fallthrough" "return")
                                :test #'string=)))))))

(defun go-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (back-to-indentation point)
    (cond ((maybe-beginning-of-comment point)
           (1+ (point-column point)))
          ((in-string-p point)
           nil)
          (t
           (let ((inside-indenting-paren nil)
                 (indent))
             (setf indent
                   (with-point ((point point))
                     (when (scan-lists point -1 1 t)
                       (case (character-at point)
                         (#\{
                          (back-to-indentation point)
                          (+ tab-width (point-column point)))
                         (#\(
                          (let ((n 0))
                            (when (with-point ((point point))
                                    (and (form-offset point -1)
                                         (member (following-word point)
                                                 '("import" "const" "var" "type" "package")
                                                 :test #'string=)))
                              (setf inside-indenting-paren t)
                              (incf n tab-width))
                            (back-to-indentation point)
                            (+ (point-column point) n)))))))
             (when (null indent)
               (return-from go-calc-indent 0))
             (cond ((looking-at point "case\\W|default\\W")
                    (decf indent tab-width))
                   ((with-point ((point point))
                      (when (looking-at (line-start point) "\\s*}")
                        (line-end point)
                        (skip-chars-backward point '(#\)))
                        (form-offset point -1)
                        (back-to-indentation point)
                        (setf indent (point-column point))))))
             (with-point ((point point))
               (line-start point)
               (skip-space-and-comment-backward point)
               (when (case (character-at point -1)
                       ((nil #\{ #\:)
                        nil)
                       (#\(
                        (not inside-indenting-paren))
                       (#\,
                        (and (scan-lists point -1 1 t)
                             (not (eql #\{ (character-at point)))))
                       (t
                        (not (semicolon-p point))))
                 (incf indent tab-width)))
             (with-point ((point point))
               (when (and (looking-at (line-start point) "^\\s*\\)\\s*$")
                          inside-indenting-paren)
                 (decf indent tab-width)))
             indent)))))

(define-key *go-mode-keymap* "}" 'go-electric-close)
(define-command go-electric-close (n) ("p")
  (self-insert n)
  (indent))

(define-command gofmt () ()
  (filter-buffer "gofmt"))

(define-command godoc (command)
    ((list (prompt-for-string "godoc ")))
  (let ((text
          (with-output-to-string (out)
            (uiop:run-program (concatenate 'string "godoc " command)
                              :output out
                              :error-output out
                              :ignore-error-status t)))
        (buffer (make-buffer "*godoc*" :read-only-p t :enable-undo-p nil)))
    (change-buffer-mode buffer 'go-mode)
    (with-pop-up-typeout-window (out buffer :erase t)
      (write-string text out))))

(defun call-godef (point)
  (let ((buffer (point-buffer point)))
    (let ((text
            (with-output-to-string (out)
              (with-input-from-string (in (points-to-string (buffer-start-point buffer)
                                                            (buffer-end-point buffer)))
                (uiop:run-program (format nil
                                          "godef -i -t -f ~A -o ~D"
                                          (probe-file (buffer-filename buffer))
                                          (position-at-point point))
                                  :input in
                                  :output out
                                  :ignore-error-status t)))))
      (with-input-from-string (in text)
        (values (read-line in nil))))))

(defun godef-successful-p (output)
  (not (or (string= "-" output)
           (string= "godef: no identifier found" output)
           (ppcre:scan '(:sequence :start-anchor "godef: no declaration found for ") output)
           (ppcre:scan '(:sequence :start-anchor "error finding import path for ") output))))

(defun godef-error (output)
  (cond ((godef-successful-p output)
         nil)
        ((string= "." output)
         "godef: expression is not defined anywhere")
        (t
         output)))

(defun godef-parse (output)
  (ppcre:register-groups-bind (filename line-number charpos)
      ("(.+):(\\d+):(\\d+)" output)
    (when (and filename line-number charpos)
      (make-xref-location :filespec filename
                          :position (cons (parse-integer line-number)
                                          (1- (parse-integer charpos)))))))

(defun find-definitions ()
  (unless (buffer-filename (current-buffer))
    (editor-error "Cannot use godef on a buffer without a file name"))
  (let ((file (call-godef (current-point))))
    (cond
      ((not (godef-successful-p file))
       (editor-error "~A" (godef-error file)))
      (t
       (godef-parse file)))))

(defun parse-gocode (text)
  (let ((json (yason:parse text)))
    (let ((len (first json))
          (candidates (second json)))
      (declare (ignore len))
      (loop :for ht :in candidates
            :for class := (gethash "class" ht)
            :for name := (gethash "name" ht)
            :for type := (gethash "type" ht)
            :collect (make-completion-item :label name
                                           :detail (format nil "~40A ~A " type class))))))

(defun gocode (point)
  (let ((buffer (point-buffer point)))
    (let ((text
            (with-output-to-string (out)
              (with-input-from-string (in (points-to-string (buffer-start-point buffer)
                                                            (buffer-end-point buffer)))
                (uiop:run-program (format nil "gocode -f=json autocomplete '~A' c~D"
                                          (or (buffer-filename buffer) "")
                                          (1- (position-at-point point)))
                                  :input in
                                  :output out)))))
      (parse-gocode text))))

(defun go-completion ()
  (gocode (current-point)))

(pushnew (cons "\\.go$" 'go-mode) *auto-mode-alist* :test #'equal)
