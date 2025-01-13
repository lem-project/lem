(uiop:define-package :lem-intelligence/lib/ollama
  (:use :cl)
  (:export :slurp
           :generate
           :chat))
(in-package :lem-intelligence/lib/ollama)

(defun hash (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun to-json (object)
  (yason:with-output-to-string* ()
    (yason:encode object)))

(defun pretty-json (object)
  (yason:with-output-to-string* (:indent t)
    (yason:encode object)))

(defun read-byte-line (stream)
  (let ((acc '()))
    (loop :for byte := (read-byte stream nil)
          :do (cond ((null byte)
                     (return (values (nreverse acc) nil)))
                    ((= byte (char-code #\newline))
                     (return (values (nreverse acc) t)))
                    (t
                     (push byte acc))))))

(defun bytes-to-string (bytes)
  (babel:octets-to-string
   (make-array (length bytes)
               :initial-contents bytes
               :element-type '(unsigned-byte 8))))

(defun slurp (result)
  (string-trim '(#\newline #\space #\")
               (with-output-to-string (out)
                 (loop :for ht :in result
                       :do (alexandria:when-let ((response (gethash "response" ht)))
                             (write-string response out))))))

(defun url (path)
  (quri:make-uri :defaults "http://localhost:11434"
                 :path path))

(defun generate (prompt &key (model (alexandria:required-argument :model)))
  (let ((response (babel:octets-to-string
                   (dex:post (url "/api/generate")
                             :headers '(("content-type" . "application/json"))
                             :content (to-json (hash "model" model "prompt" prompt))
                             :read-timeout nil
                             :connect-timeout nil))))
    (with-input-from-string (input-stream response)
      (loop :for object := (handler-case (yason:parse input-stream)
                             (end-of-file () nil))
            :while object
            :collect object))))

(defun chat (&key (role nil role-p)
                  (content (alexandria:required-argument :content))
                  (model (alexandria:required-argument :model))
                  (callback (alexandria:required-argument :callback)))
  (let ((stream
          (dex:post (url "/api/chat")
                    :headers '(("content-type" . "application/json"))
                    :content (to-json (hash "model" model
                                            "messages" (list
                                                        (if role-p
                                                            (hash "role" role
                                                                  "content" content)
                                                            (hash "content" content)))))
                    :want-stream t
                    :read-timeout nil
                    :connect-timeout nil)))
    (loop
      (multiple-value-bind (bytes continue)
          (read-byte-line stream)
        (when bytes
          (funcall callback (yason:parse (bytes-to-string bytes))))
        (unless continue
          (return))))))
