(in-package :lem-tests)

(defmacro define-indent-test (name before &optional (after before))
  (let ((name (if (eq name '*)
                  (generate-anonymous-test-name "INDENT-")
                  name)))
    `(define-test ,name
       (run-indent-test ',name ,before ,after))))

(defun run-indent-test (name before-text after-text)
  (let ((buffer (lem:make-buffer (format nil "*indent-test ~A*" name)
                                 :syntax-table lem-lisp-syntax:*syntax-table*
                                 :temporary t)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem:erase-buffer buffer)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:insert-string p before-text))
    (lem:indent-buffer buffer)
    (test (string= after-text (lem:buffer-text buffer))
          (report name
                  (lem:buffer-text buffer)
                  after-text))))

(defun diff-text (text1 text2)
  (string-trim
   '(#\newline #\space #\tab)
   (with-output-to-string (out)
     (with-input-from-string (in1 text1)
       (with-input-from-string (in2 text2)
         (loop :with eof-value := '#:eof
               :for line1 := (read-line in1 nil eof-value)
               :for line2 := (read-line in2 nil eof-value)
               :until (eq line1 eof-value)
               :do (cond ((string= line1 line2)
                          (format out " ~A~%" line1))
                         (t
                          (write-string (cl-ansi-text:green (format nil "+~A~%" line1)) out)
                          (write-string (cl-ansi-text:red (format nil "-~A~%" line2)) out)))))))))

(defun report (name before-text after-text)
  (format nil "# error: ~A~%~A~%"
          name
          (diff-text before-text after-text)))


(define-indent-test cond-1
"
(cond ((foo 1
            2)))
")

(define-indent-test defclass-1
  "
`(defclass foo ()
   ,(mapcar x
            y))
")

(define-indent-test *
  "
aaaaaaa(
        a
        b
        c
        )
")

(define-indent-test *
  "
(defun foo (foo &optional opt1
                          opt2
                &rest rest)
  (list foo opt1 opt2
        rest))
")

(define-indent-test *
  "
(defmacro foo ((foo &optional opt1
                              opt2
                    &rest rest))
  (list foo opt1 opt2
        rest))
")

(define-indent-test *
  "
(#+sbcl foo
 #+ccl bar)
")

(define-indent-test *
  "
(let ((x y)
      (foo #-foo (no-foo)
           #+foo (yes-foo))
      (bar #-bar
           (no-bar)
           #+bar
           (yes-bar)))
  (list foo bar
        x))
")

(define-indent-test *
  "
(list ;comment
 foo
 bar)
")

(define-indent-test *
  "
(defun foo (x)
  (tagbody
   foo
    (bar)
   baz
    (when (losing)
      (with-big-loser
          (yow)
        ((lambda ()
           foo)
         big)))
    (flet ((foo (bar baz zap)
             (zip))
           (zot ()
             quux))
      (do ()
          ((lose)
           (foo 1))
        (quux)
        foo
        (lose))
      (cond ((x)
             (win 1 2
                  (foo)))
            (t
             (lose
              3)))))))
")

(define-indent-test *
  "
(defmacro foo (body)
  `(let (,@(stuff)
         ,(more-stuff)
         ,(even-more)
         (foo foo))
     ,@bofy))
")

(define-indent-test *
  "
(defun foo ()
  `(list foo bar
         ,@(quux fo
                 foo)))
")

(define-indent-test *
  "
(defmacro foofoo (body)
  `(foo
    `(let (,',@,(stuff)
           ,(more-stuff)
           ,(even-more)
           (foo foo))
       ,@bofy)))
")

(define-indent-test *
  "
(loop for i from 0 below 2
      for j from 0 below 2
      when foo
      do (fubar)
         (bar)
         (moo)
      and collect cash
      into honduras
      else do ;; this is the body of the first else
              ;; the body is ...
         (indented to the above comment)
         (ZMACS gets this wrong)
      and do this
      and do that
      and when foo
      do the-other
      and cry
      when this-is-a-short-condition do
         (body code of the when)
      when here's something I used to botch do (here is a body)
                                               (rest of body indented same)
      do
         (exdented loop body)
         (I'm not sure I like this but it's compatible)
      when funny-predicate do ;; Here's a comment
         (body filled to comment))
")

(define-indent-test *
  "
(loop for x in foo1
      for y in quux1
      )
")

(define-indent-test *
  "
(loop for x in foo
      for y in quux
      finally (foo)
              (fo)
              (zoo)
      do
         (print x)
         (print y)
         (print 'ok!))
")

(define-indent-test *
  "
(loop for f in files
      collect (open f
                    :direction :output)
      do (foo) (bar)
         (quux))
")

(define-indent-test *
  "
(loop (foo)
      ;; comment
      (bar)
      (quux))
")

(define-indent-test *
  "
(loop ;; comment
      (foo)
      (bar))
")

(define-indent-test *
  "
(loop
  (foo)
  ;; comment
  (bar))
")

(define-indent-test *
  "
(loop
  ;; comment
  (foo)
  (bar))
")

(define-indent-test *
  "
(loop ;; comment at toplevel of the loop
      with foo = t
      do (foo foo)
         (foo))
")

(define-indent-test *
  "
(loop
  ;; comment at toplevel of the loop
  with foo = t
  do (foo foo))
")

(define-indent-test *
  "
(loop
  ;; comment at toplevel of the loop
  with foo = t
  do (foo foo)
     (foo))
")

(define-indent-test *
  "
(loop with foo = t
      do (foo foo)
         ;; comment inside clause
         (bar))
")

(define-indent-test *
  "
(progn
  (loop
    repeat 1000
    do ;; This is the
       ;; beginning
       (foo))
  (loop repeat 100 ;; This too
                   ;; is a beginning
        do (foo)))
")

(define-indent-test *
  "
(loop
  for foo in bar
  do
     (list foo
           bar
           baz))
")

(define-indent-test *
  "
(defmethod foo
    ()
  ()
  ())
")

(define-indent-test *
  "
(defmethod foo :after
    ()
  ()
  ())
")

(define-indent-test *
  "
(defgeneric foo ()
  (:method
      ()
    1
    2))
")

(define-indent-test *
  "
(defgeneric foo ()
  (:method :after
      ()
    1
    2))
")

(define-indent-test *
  "
(defun foo (a
            &optional b
                      c))
")

(define-indent-test *
  "
(defun foo (a
            &optional
            b
            c))
")

(define-indent-test *
  "
(defun foo (a &optional
              b
              c))
")

(defun indent-test-for-file (pathname)
  (let ((buffer (lem:find-file-buffer pathname :temporary t :enable-undo-p nil)))
    (setf (lem:variable-value 'lem:calc-indent-function :buffer buffer)
          'lem-lisp-syntax:calc-indent)
    (lem:with-point ((p (lem:buffer-point buffer)))
      (lem:buffer-start p)
      (loop
        (lem:with-point ((start p))
          (unless (lem:form-offset p 1)
            (return))
          (let ((text (lem:points-to-string start p))
                (name (format nil "~A:~D" pathname (lem:line-number-at-point start))))
            (run-indent-test name text text)))))))

(defun indent-test-for-system (system-name)
  (dolist (pathname (directory
                     (merge-pathnames "*.lisp"
                                      (asdf:system-source-directory system-name))))
    (indent-test-for-file pathname)))

(define-test indent-test-under-lem-base
  (indent-test-for-system :lem-base))
