;; -*- mode: lisp; package: repl -*-

(in-package :cl-user)

(defpackage leval-server
  (:use :cl)
  (:export
   :server-start
   :repl))

(in-package :leval-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun common-prefix (items)
  (subseq (car items)
          0
          (apply #'min
                 (mapcar #'(lambda (item)
                             (or (mismatch (car items) item)
                                 (length item)))
                         (cdr items)))))

(defun package-prefix (str)
  (cond ((let ((pos (search "::" str)))
           (when pos
             (list (subseq str (+ pos 2)) (subseq str 0 pos) nil))))
        ((let ((pos (position #\: str)))
           (when pos
             (list (subseq str (+ pos 1))
                   (if (zerop pos)
                       "KEYWORD"
                       (subseq str 0 pos))
                   t))))
        (t
         (list str nil nil))))

(defun symbol-complete (text)
  (let ((text (string-upcase text))
        (els))
    (flet ((body (sym text prefix)
                 (let ((name (string sym)))
                   (when (eql 0 (search text name))
                     (push (format nil "~(~a~a~)" prefix name)
                           els)))))
      (destructuring-bind (symbol-name package external-p)
          (package-prefix text)
        (cond ((and package external-p)
               (do-external-symbols (sym package)
                 (body sym symbol-name
                       (if (equal (package-name :keyword)
                                  (package-name package))
                           ":"
                           (format nil "~a:" package)))))
              (package
               (do-symbols (sym package)
                 (body sym symbol-name (format nil "~a::" package))))
              (t
               (do-symbols (sym *package*)
                 (body sym symbol-name ""))
               (dolist (package (list-all-packages))
                 (body (format nil "~a:" (package-name package))
                       symbol-name "")
                 (dolist (package-name (package-nicknames package))
                   (body (format nil "~a:" package-name)
                         symbol-name "")))))))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun arglist (symbol)
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

(defmacro case-dbind (event &body cases)
  (let ((gevent (make-symbol "EVENT")))
    `(let ((,gevent ,event))
       (ecase (car ,gevent)
         ,@(mapcar #'(lambda (c)
                       `(,(caar c)
                         (destructuring-bind ,(cdar c)
                             (cdr ,gevent)
                           ,@(cdr c))))
                   cases)))))

(defun safe-read-from-string (string &optional (package *package*))
  (multiple-value-bind (x condition)
      (ignore-errors
       (let ((*package* package))
         (values (read-from-string string))))
    (values x condition)))

(defun read-and-macroexpand (string macroexpand-function package-name stream)
  (let ((*package* (find-package package-name)))
    (multiple-value-bind (x condition)
        (safe-read-from-string string)
      (if condition
          (print nil stream)
          (print (prin1-to-string (funcall macroexpand-function x))
                 stream))
      (force-output stream))))

(defun read-symbol-and-apply (string apply-function package-name stream)
  (let ((*package* (find-package package-name)))
    (multiple-value-bind (x condition)
        (safe-read-from-string string)
      (if condition
          (print nil stream)
          (print (with-output-to-string (out)
                   (funcall apply-function x out))
                 stream))
      (force-output stream))))

(defun dispatch-event (event stream)
  (case-dbind event
              ((:eval string package-name)
               (multiple-value-bind (x condition)
                   (safe-read-from-string string (find-package package-name))
                 (cond (condition
                        (format stream "~&~a~%" condition)
                        (force-output stream))
                       (t
                        (multiple-value-bind (values condition)
                            (ignore-errors
                             (multiple-value-list
                              (let ((*package* (find-package package-name)))
                                (eval x))))
                          (print (or condition
                                     (mapcar #'prin1-to-string values))
                                 stream)
                          (force-output stream))))))
              ((:find-package string)
               (let ((package (find-package string)))
                 (print (if package
                            (package-name package)
                            nil)
                        stream)
                 (force-output stream)))
              ((:macroexpand-1 string package-name)
               (read-and-macroexpand string
                                     #'macroexpand-1
                                     package-name
                                     stream))
              ((:macroexpand string package-name)
               (read-and-macroexpand string
                                     #'macroexpand
                                     package-name
                                     stream))
              ((:complete-symbol string package-name)
               (let ((*package* (find-package package-name)))
                 (print (symbol-complete string) stream)
                 (force-output stream)))
              ((:describe-symbol string package-name)
               (read-symbol-and-apply string
                                      #'(lambda (x out)
                                          (describe x out))
                                      package-name
                                      stream))
              ((:disassemble-symbol string package-name)
               (read-symbol-and-apply string
                                      #'(lambda (x out)
                                          (disassemble x :stream out))
                                      package-name
                                      stream))
              ((:trace string untrace-p package-name)
               (let ((symbol (if string (safe-read-from-string string))))
                 (when (symbolp symbol)
                   (cond ((null untrace-p)
                          (format t "~&Trace ~a~%" symbol)
                          (eval `(trace ,symbol)))
                         ((not (null symbol))
                          (format t "~&Untrace ~a~%" symbol)
                          (eval `(untrace ,symbol)))
                         (t
                          (format t "~&Untrace all~%")
                          (untrace))))
                 (print t stream)
                 (force-output stream)))
              ((:arglist string package-name)
               (let ((package (find-package package-name)))
                 (multiple-value-bind (symbol condition)
                     (safe-read-from-string string package)
                   (if condition
                       (print nil stream)
                       (let ((x (arglist symbol)))
                         (cond (x
                                (print x stream))
                               ((symbolp symbol)
                                (unintern symbol package)
                                (print nil stream)))))
                   (force-output stream))))))

(defun server-start (hostname port)
  (format t "~&hostname: ~a~%port: ~a~%" hostname port)
  (bt:make-thread
   #'(lambda ()
       (usocket:with-socket-listener (socket hostname port :reuse-address t)
         (loop
           (usocket:with-connected-socket (connected-socket
                                           (usocket:socket-accept socket))
             (let* ((stream (usocket:socket-stream connected-socket))
                    (event (read stream)))
               (dispatch-event event stream))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(rl:register-function :complete #'(lambda (text start end)
                                    (declare (ignore start end))
                                    (symbol-complete text)))

(defun add-history (str)
  (cffi:foreign-funcall "add_history"
                        :string str
                        :void))

(defun add-history-expr (x)
  (add-history (string-downcase (prin1-to-string x))))

(defun readline-read (prompt)
  (let ((line (rl:readline :prompt prompt)))
    (loop
      :with x :and pos
      :for error-p := nil :do
      (handler-case (setf (values x pos)
                          (read-from-string line nil))
        (error () (setq error-p t)))
      (cond (error-p
             (setq line
                   (concatenate 'string line " "
                                (rl:readline :already-prompted t))))
            (t
             (add-history-expr x)
             (return x))))))

(let (values)
  (defun eval-print (-)
    (setq values
          (multiple-value-list (eval -)))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (mapc #'pprint values)
    (terpri))
  (defun repl ()
    (loop
      (restart-case (eval-print
                     (setq - (readline-read
                              (format nil "~&~a> "
                                      (package-name *package*)))))
        (restart-toplevel () :report "Restart toplevel.")))))
