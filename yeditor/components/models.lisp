;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/models
  (:use :cl)
  (:export :provider
           :provider-name
           :provider-base-url
           :provider-default-model
           :provider-cloud-p
           :ollama-provider
           :openai-compatible-provider
           :claude-provider
           :provider-available-p
           :provider-models
           :provider-generate
           :ollama-pull
           :*allow-cloud*
           :cloud-provider-disabled
           :provider-error
           :provider-error-status
           :generation-refused))
(in-package :yeditor-components/models)

;;;; le-models (plan T16): one interface over AI providers.
;;;;   ollama-provider            local Ollama (default, http://127.0.0.1:11434)
;;;;   openai-compatible-provider any /v1/chat/completions server (LM Studio, vLLM, hosted APIs)
;;;;   claude-provider            the Claude API (cloud; key from ANTHROPIC_API_KEY)
;;;; Privacy: local is the default; a provider that leaves the machine is refused unless
;;;; *allow-cloud* is true. API keys are read from the environment, never stored.

(defvar *allow-cloud* nil
  "Opt-in for providers that send code off the machine.")

(define-condition cloud-provider-disabled (error)
  ((provider :initarg :provider :reader cloud-provider-disabled-provider))
  (:report (lambda (c s)
             (format s "~A sends code off this machine; enable cloud providers to use it"
                     (provider-name (cloud-provider-disabled-provider c))))))

(define-condition provider-error (error)
  ((status :initarg :status :initform nil :reader provider-error-status)
   (message :initarg :message :reader provider-error-message))
  (:report (lambda (c s)
             (format s "AI provider error~@[ (HTTP ~A)~]: ~A"
                     (provider-error-status c) (provider-error-message c)))))

(define-condition generation-refused (provider-error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "The model declined this request (stop_reason: refusal)"))))

(defclass provider ()
  ((name :initarg :name :reader provider-name)
   (base-url :initarg :base-url :accessor provider-base-url)
   (default-model :initarg :default-model :accessor provider-default-model)))

(defgeneric provider-cloud-p (provider)
  (:documentation "True when requests leave this machine.")
  (:method ((provider provider))
    (let ((host (quri:uri-host (quri:uri (provider-base-url provider)))))
      (not (member host (list "127.0.0.1" "localhost" "::1") :test (function equal))))))

(defgeneric provider-available-p (provider)
  (:documentation "True when the provider answers (short timeout)."))
(defgeneric provider-models (provider)
  (:documentation "Names of the models the provider offers."))
(defgeneric provider-generate (provider prompt &key model system on-chunk)
  (:documentation "Generate a completion for PROMPT, calling ON-CHUNK with each streamed
   piece of text; returns the whole text."))

;;; HTTP helpers

(defun url (provider path)
  (concatenate (quote string) (string-right-trim "/" (provider-base-url provider)) path))

(defun json (object)
  (with-output-to-string (s) (yason:encode object s)))

(defun object (&rest keys-and-values)
  (let ((table (make-hash-table :test (function equal))))
    (loop :for (k v) :on keys-and-values :by (function cddr) :do (setf (gethash k table) v))
    table))

(defun get-json (provider path &key headers (timeout 3))
  (check-cloud provider)
  (yason:parse (dex:get (url provider path) :headers headers :connect-timeout timeout
                                            :read-timeout timeout :keep-alive nil)))

(defun retryable-status-p (status)
  (or (eql status 429) (eql status 529) (and status (>= status 500))))

(defun error-message (body)
  (or (ignore-errors (gethash "message" (gethash "error" (yason:parse body))))
      (ignore-errors (gethash "error" (yason:parse body)))
      body))

(defun call-with-retries (function &key (attempts 3))
  "Call FUNCTION; on 429, 529 or 5xx retry (honouring retry-after, capped at 10 s),
   up to ATTEMPTS times. Other HTTP errors become provider-error immediately."
  (loop :for attempt :from 1
        :do (handler-case (return (funcall function))
              (dex:http-request-failed (e)
                (let ((status (dex:response-status e)))
                  (if (and (retryable-status-p status) (< attempt attempts))
                      (sleep (min 10 (or (ignore-errors
                                          (parse-integer (gethash "retry-after" (dex:response-headers e))))
                                         (expt 2 (1- attempt)))))
                      (error (quote provider-error) :status status
                                                    :message (error-message (dex:response-body e)))))))))

(defun post-lines (provider path body &key headers)
  "POST BODY as JSON and return a UTF-8 character stream over the streamed response."
  (check-cloud provider)
  (let ((raw (call-with-retries
              (lambda ()
                (dex:post (url provider path)
                          :content (json body)
                          :headers (append (list (cons "content-type" "application/json")) headers)
                          :want-stream t :force-binary t :keep-alive nil
                          :connect-timeout 5 :read-timeout 300)))))
    (flexi-streams:make-flexi-stream raw :external-format :utf-8)))

(defmacro do-lines ((line stream-form) &body body)
  "Evaluate STREAM-FORM once, run BODY for each line of the stream, then close it."
  (let ((stream (gensym "STREAM")))
    `(let ((,stream ,stream-form))
       (unwind-protect
            (loop :for ,line := (read-line ,stream nil)
                  :while ,line
                  :do (progn ,@body))
         (close ,stream)))))

(defun collect-chunks (on-chunk)
  "Returns (values push-function result-function) accumulating streamed text."
  (let ((out (make-string-output-stream)))
    (values (lambda (text)
              (when (and text (plusp (length text)))
                (write-string text out)
                (when on-chunk (funcall on-chunk text))))
            (lambda () (get-output-stream-string out)))))

(defun check-cloud (provider)
  (when (and (provider-cloud-p provider) (not *allow-cloud*))
    (error (quote cloud-provider-disabled) :provider provider)))

;;; Ollama

(defclass ollama-provider (provider) ()
  (:default-initargs :name "ollama" :base-url "http://127.0.0.1:11434" :default-model "llama3.2"))

(defmethod provider-available-p ((p ollama-provider))
  (and (ignore-errors (get-json p "/api/version" :timeout 1)) t))

(defmethod provider-models ((p ollama-provider))
  (mapcar (lambda (m) (gethash "name" m)) (gethash "models" (get-json p "/api/tags"))))

(defmethod provider-generate ((p ollama-provider) prompt &key model system on-chunk)
  (check-cloud p)
  (multiple-value-bind (push result) (collect-chunks on-chunk)
    (do-lines (line (post-lines p "/api/generate"
                                (apply (function object)
                                       "model" (or model (provider-default-model p))
                                       "prompt" prompt "stream" t
                                       (when system (list "system" system)))))
      (let ((data (yason:parse line)))
        (when (gethash "error" data)
          (error (quote provider-error) :message (gethash "error" data)))
        (funcall push (gethash "response" data))))
    (funcall result)))

(defun ollama-pull (provider model &key on-status)
  "Download MODEL into Ollama, calling ON-STATUS with each progress status."
  (do-lines (line (post-lines provider "/api/pull" (object "name" model "stream" t)))
    (let ((data (yason:parse line)))
      (when (gethash "error" data)
        (error (quote provider-error) :message (gethash "error" data)))
      (when on-status (funcall on-status (gethash "status" data)))))
  model)

;;; OpenAI-compatible

(defclass openai-compatible-provider (provider)
  ((api-key-env :initarg :api-key-env :initform "OPENAI_API_KEY" :reader api-key-env))
  (:default-initargs :name "openai-compatible" :base-url "http://127.0.0.1:8080" :default-model "local-model"))

(defun bearer (provider)
  (let ((key (uiop:getenv (api-key-env provider))))
    (and key (list (cons "authorization" (format nil "Bearer ~A" key))))))

(defmethod provider-available-p ((p openai-compatible-provider))
  (and (ignore-errors (get-json p "/v1/models" :timeout 1 :headers (bearer p))) t))

(defmethod provider-models ((p openai-compatible-provider))
  (mapcar (lambda (m) (gethash "id" m)) (gethash "data" (get-json p "/v1/models" :headers (bearer p)))))

(defun sse-data (line)
  "The payload of an SSE 'data:' LINE, or nil."
  (and (uiop:string-prefix-p "data:" line)
       (string-trim " " (subseq line 5))))

(defmethod provider-generate ((p openai-compatible-provider) prompt &key model system on-chunk)
  (check-cloud p)
  (multiple-value-bind (push result) (collect-chunks on-chunk)
    (do-lines (line (post-lines p "/v1/chat/completions"
                                (object "model" (or model (provider-default-model p))
                                        "stream" t
                                        "messages" (append (when system
                                                             (list (object "role" "system" "content" system)))
                                                           (list (object "role" "user" "content" prompt))))
                                :headers (bearer p)))
      (let ((data (sse-data line)))
        (when (and data (string/= data "[DONE]"))
          (let ((choice (first (gethash "choices" (yason:parse data)))))
            (funcall push (and choice (gethash "content" (gethash "delta" choice))))))))
    (funcall result)))

;;; Claude (Messages API over raw HTTP; Common Lisp has no official SDK)

(defclass claude-provider (provider)
  ((max-tokens :initarg :max-tokens :initform 64000 :reader claude-max-tokens))
  (:default-initargs :name "claude" :base-url "https://api.anthropic.com" :default-model "claude-opus-5-5"))

(defmethod provider-cloud-p ((p claude-provider))
  ;; the Claude API is always remote, even behind a local proxy
  t)

(defun claude-headers ()
  (let ((key (uiop:getenv "ANTHROPIC_API_KEY")))
    (unless key
      (error (quote provider-error) :message "ANTHROPIC_API_KEY is not set"))
    (list (cons "x-api-key" key)
          (cons "anthropic-version" "2023-06-01")
          ;; route policy declines to a fallback model inside the same call
          (cons "anthropic-beta" "server-side-fallback-2026-07-01"))))

(defmethod provider-available-p ((p claude-provider))
  (and (uiop:getenv "ANTHROPIC_API_KEY") t))

(defmethod provider-models ((p claude-provider))
  (list (provider-default-model p)))

(defmethod provider-generate ((p claude-provider) prompt &key model system on-chunk)
  (check-cloud p)
  (let ((headers (claude-headers))
        (stop-reason nil))
    (multiple-value-bind (push result) (collect-chunks on-chunk)
      (do-lines (line (post-lines p "/v1/messages"
                                  (apply (function object)
                                         "model" (or model (provider-default-model p))
                                         "max_tokens" (claude-max-tokens p)
                                         "stream" t
                                         "fallbacks" "default"
                                         "messages" (list (object "role" "user" "content" prompt))
                                         (when system (list "system" system)))
                                  :headers headers))
        (let ((data (sse-data line)))
          (when data
            (let* ((event (yason:parse data))
                   (type (gethash "type" event)))
              (cond ((equal type "content_block_delta")
                     (let ((delta (gethash "delta" event)))
                       ;; only text deltas are output; thinking deltas are not shown
                       (when (equal (gethash "type" delta) "text_delta")
                         (funcall push (gethash "text" delta)))))
                    ((equal type "message_delta")
                     (setf stop-reason (gethash "stop_reason" (gethash "delta" event))))
                    ((equal type "error")
                     (error (quote provider-error)
                            :message (gethash "message" (gethash "error" event)))))))))
      (when (equal stop-reason "refusal")
        (error (quote generation-refused) :message "refusal"))
      (funcall result))))
