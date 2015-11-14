;; -*- mode: lisp; package: repl -*-

(in-package :cl-user)

(defpackage leval-server
  (:use :cl)
  (:export
   :server-start))

(in-package :leval-server)

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

(leval-server:server-start "localhost" 53912)
