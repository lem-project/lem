(cl-lsp/defpackage:defpackage :cl-lsp/eval
  (:use :cl
        :cl-lsp/logger
        :cl-lsp/methods
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/slime
        :cl-lsp/swank
        :cl-lsp/gray-streams
        :cl-lsp/server)
  (:import-from :cl-lsp/methods/lifetime
                :register-initialized-hook)
  (:import-from :lem-base
                :with-point
                :points-to-string))
(in-package :cl-lsp/eval)

(defvar *eval-thread* nil)

(let ((wait (bt:make-condition-variable))
      (lock (bt:make-lock))
      (queue (list)))

  (defun receive ()
    (bt:with-lock-held (lock)
      (bt:condition-wait wait lock)
      (pop queue)))

  (defun send (x)
    (bt:with-lock-held (lock)
      (setf queue (nconc queue (list x)))
      (bt:condition-notify wait))))

(defun ensure-package (package)
  (or (find-package package)
      (find-package "CL-USER")))

(defun start-eval-thread ()
  (unless *eval-thread*
    (setf *eval-thread*
          (bt:make-thread
           (lambda ()
             (with-error-handle
               (loop :for event := (receive) :do
                 (funcall event))))))))

(register-initialized-hook 'start-eval-thread)

(defun send-eval (function)
  (jsonrpc:notify-async *server* "lisp/evalBegin" nil)
  (send (lambda ()
          (funcall function)
          (bt:with-lock-held (*method-lock*)
            (jsonrpc:notify-async *server* "lisp/evalEnd" nil)))))

(defun lsp-output-fn (string)
  (bt:with-lock-held (*method-lock*)
    (notify-log-message |MessageType.Log| string)))

(defun call-with-eval-stream (function)
  (let ((out (make-instance 'lsp-output-stream :output-fn #'lsp-output-fn)))
    (with-input-from-string (in "")
      (with-open-stream (eval-stream (make-two-way-stream in out))
        (let ((*standard-output* eval-stream)
              (*error-output* eval-stream)
              (*standard-input* eval-stream)
              (*terminal-io* eval-stream)
              (*query-io* eval-stream)
              (*debug-io* eval-stream)
              (*trace-output* eval-stream))
          (funcall function eval-stream))))))

(defmacro with-eval-stream ((stream-var) &body body)
  `(call-with-eval-stream (lambda (,stream-var) ,@body)))

(defun call-with-muffle-streams (function)
  (let ((stream (make-broadcast-stream)))
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*standard-input* stream)
          (*terminal-io* stream)
          (*query-io* stream)
          (*debug-io* stream)
          (*trace-output* stream))
      (funcall function))))

(defmacro with-muffle-streams (() &body body)
  `(call-with-muffle-streams (lambda () ,@body)))


(defun compilation-notes-to-diagnostics (notes)
  (let ((diagnostics '()))
    (compilation-notes
     notes
     (lambda (start end severity message)
       (push (make-instance '|Diagnostic|
                            :|range| (make-lsp-range start end)
                            :|severity| (case severity
                                          ((:error :read-error)
                                           |DiagnosticSeverity.Error|)
                                          ((:warning :style-warning)
                                           |DiagnosticSeverity.Warning|)
                                          ((:note :redefinition)
                                           |DiagnosticSeverity.Information|))
                            ;; :|code|
                            ;; :|source|
                            :|message| message)
             diagnostics)))
    (list-to-object[] diagnostics)))

(defun compilation-message (notes secs successp)
  (with-output-to-string (out)
    (if successp
        (princ "Compilation finished" out)
        (princ "Compilation failed" out))
    (princ (if (null notes)
               ". (No warnings)"
               ". ")
           out)
    (when secs
      (format nil "[~,2f secs]" secs))))

(defun compile-and-load-file (uri)
  (let ((filename (uri-to-filename uri))
        result)
    (handler-case (with-muffle-streams ()
                    (setf result (swank-compile-file filename t)))
      (error (c)
        (bt:with-lock-held (*method-lock*)
          (notify-show-message |MessageType.Error|
                               (princ-to-string c)))
        (setf result nil)))
    (when result
      (destructuring-bind (notes successp duration loadp fastfile)
          (rest result)
        (bt:with-lock-held (*method-lock*)
          (notify-show-message |MessageType.Info|
                               (compilation-message
                                notes duration successp))
          (jsonrpc:notify-async
           *server*
           "textDocument/publishDiagnostics"
           (convert-to-hash-table
            (make-instance '|PublishDiagnosticsParams|
                           :|uri| uri
                           :|diagnostics| (compilation-notes-to-diagnostics notes)))))
        (when (and loadp fastfile successp)
          (handler-case
              (with-eval-stream (eval-stream)
                (load fastfile)
                (finish-output eval-stream))
            (error (condition)
              (bt:with-lock-held (*method-lock*)
                (notify-show-message |MessageType.Error|
                                     (princ-to-string condition))))))))))

(define-method "lisp/compileAndLoadFile" (params |TextDocumentIdentifier|) ()
  (let* ((uri (slot-value params '|uri|)))
    (send-eval (lambda () (compile-and-load-file uri))))
  nil)

(defun eval-string (string package)
  (let ((*package* (ensure-package package))
        results)
    (with-eval-stream (eval-stream)
      (handler-bind
          ((error (lambda (err)
                    (finish-output eval-stream)
                    (bt:with-lock-held (*method-lock*)
                      (notify-log-message |MessageType.Error|
                                          (with-output-to-string (out)
                                            (format out "~%~A~%~%" err)
                                            (uiop:print-backtrace :stream out)))
                      (notify-show-message |MessageType.Error|
                                           (princ-to-string err)))
                    (return-from eval-string))))
        (setf results
              (multiple-value-list
               (eval (read-from-string string)))))
      (finish-output eval-stream)
      (bt:with-lock-held (*method-lock*)
        (notify-show-message |MessageType.Info| (format nil "~{~A~^, ~}" results))))))

(defun send-eval-string (string package)
  (send-eval (lambda () (eval-string string package))))

(define-method "lisp/eval" (params |TextDocumentPositionParams|) ()
  (with-text-document-position (point) params
    (let ((string (form-string point)))
      (when string
        (let ((package (search-buffer-package point)))
          (send-eval-string string package)))
      nil)))

(define-method "lisp/rangeEval" (params) ()
  (let* ((uri (gethash "uri" (gethash "textDocument" params)))
         (range (convert-from-hash-table '|Range| (gethash "range" params))))
    (with-slots (|start| |end|) range
      (with-document-position (start uri |start|)
        (with-point ((end start))
          (move-to-lsp-position end |end|)
          (send-eval-string (points-to-string start end)
                            (search-buffer-package start)))))))

(define-method "lisp/interrupt" (params nil) (:without-lock t)
  (when *eval-thread*
    (bt:interrupt-thread *eval-thread*
                         (lambda ()
                           (error "interrupt"))))
  nil)
