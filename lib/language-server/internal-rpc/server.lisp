(defpackage :lem-language-server/internal-rpc/server
  (:use :cl)
  (:export :start-server))
(in-package :lem-language-server/internal-rpc/server)

(defvar *io-package*
  (make-package :lem-language-server/internal-rpc/internal-package
                :use '()))

(defvar *message-dispatcher* (make-hash-table :test 'eq))

(defun safe-read-from-string (string &optional (package *io-package*))
  (let ((*package* package)
        (*read-suppress* nil)
        (*read-eval* nil))
    (read-from-string string)))

(defun write-message (message &optional (stream *standard-output*))
  (let* ((content-length (length message)))
    (format stream "~6,'0,X" content-length)
    (write-string message stream)
    (values)))

(defun read-message (&optional (stream *standard-input*))
  (flet ((read-content-length ()
           (let ((buffer (make-array 6 :element-type 'character)))
             (read-sequence buffer stream)
             (parse-integer buffer :radix 16))))
    (let* ((length (read-content-length))
           (buffer (make-array length :element-type 'character)))
      (read-sequence buffer stream)
      (safe-read-from-string buffer))))

(defun process-message (message)
  (when (consp message)
    (let ((dispatcher (gethash (first message) *message-dispatcher*)))
      (when dispatcher
        (funcall dispatcher message)))))

(defun start-server ()
  (let ((output-stream (make-broadcast-stream)))
    (let ((*standard-output* output-stream)
          (*error-output* output-stream)
          (*trace-output* output-stream))
      (loop :for message := (read-message)
            :do (process-message message)))))

(defmacro define-message (name (&rest parameters) &body body)
  (let ((message (gensym "MESSAGE")))
    `(setf (gethash ,name *message-dispatcher*)
           (lambda (,message)
             (destructuring-bind (,@parameters) (rest ,message)
               ,@body)))))

(define-message :describe-symbol (symbol-name package-name)
  (let ((package (find-package package-name)))
    (when package
      (let* ((symbol (safe-read-from-string symbol-name package))
             (text (with-output-to-string (output)
                     (describe symbol output))))
        `(:result ,text)))))

(defun test ()
  (let ((message (with-output-to-string (stream)
                   (write-message (prin1-to-string '(:describe-symbol "DEFUN" "CL-USER"))
                                  stream))))
    (assert (equal "000024(:DESCRIBE-SYMBOL \"DEFUN\" \"CL-USER\")" message))
    (with-input-from-string
        (stream message)
      (assert (equal '(:RESULT "COMMON-LISP:DEFUN
  [symbol]

DEFUN names a macro:
  Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
  Documentation:
    Define a function at top level.
  Source file: SYS:SRC;CODE;MACROS.LISP
")
                     (process-message (read-message stream)))))))
