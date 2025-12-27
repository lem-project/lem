(defpackage :lem-tests/language-server/utils
  (:use :cl
        :lem-language-server)
  (:import-from :lem-lsp-base/type
                :make-lsp-map)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json)
  (:export :lines
           ;; request helpers
           :call-completion-request
           :call-hover-request
           :call-definition-request
           :call-references-request
           :call-document-symbol-request
           :call-formatting-request
           :call-document-highlight-request
           :call-signature-help-request
           ;; position helpers
           :make-position
           :make-range
           :make-text-document-identifier
           :make-text-document-position-params))
(in-package :lem-tests/language-server/utils)

(defun lines (&rest strings)
  (format nil "~{~A~%~}" strings))

;;; Position and range helpers

(defun make-position (line character)
  "Create an LSP position object."
  (make-instance 'lsp:position :line line :character character))

(defun make-range (start-line start-char end-line end-char)
  "Create an LSP range object."
  (make-instance 'lsp:range
                 :start (make-position start-line start-char)
                 :end (make-position end-line end-char)))

(defun make-text-document-identifier (uri)
  "Create an LSP text document identifier."
  (make-instance 'lsp:text-document-identifier :uri uri))

(defun make-text-document-position-params (uri line character)
  "Create text document position params for LSP requests."
  (make-instance 'lsp:text-document-position-params
                 :text-document (make-text-document-identifier uri)
                 :position (make-position line character)))

;;; Request helper functions

(defun call-completion-request (uri &key (line 0) (character 0))
  "Call the completion request and return the response."
  (call-lsp-method
   (make-instance 'completion-request)
   (convert-to-json
    (make-instance 'lsp:completion-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)))))

(defun call-hover-request (uri &key (line 0) (character 0))
  "Call the hover request and return the response."
  (call-lsp-method
   (make-instance 'hover-request)
   (convert-to-json
    (make-instance 'lsp:hover-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)))))

(defun call-definition-request (uri &key (line 0) (character 0))
  "Call the go-to-definition request and return the response."
  (call-lsp-method
   (make-instance 'go-to-definition-request)
   (convert-to-json
    (make-instance 'lsp:definition-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)))))

(defun call-references-request (uri &key (line 0) (character 0) (include-declaration t))
  "Call the find-references request and return the response."
  (call-lsp-method
   (make-instance 'find-references-request)
   (convert-to-json
    (make-instance 'lsp:reference-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)
                   :context (make-instance 'lsp:reference-context
                                           :include-declaration include-declaration)))))

(defun call-document-symbol-request (uri)
  "Call the document symbol request and return the response."
  (call-lsp-method
   (make-instance 'document-symbol-request)
   (convert-to-json
    (make-instance 'lsp:document-symbol-params
                   :text-document (make-text-document-identifier uri)))))

(defun call-formatting-request (uri &key (tab-size 4) (insert-spaces t))
  "Call the document formatting request and return the response."
  (call-lsp-method
   (make-instance 'document-formatting-request)
   (convert-to-json
    (make-instance 'lsp:document-formatting-params
                   :text-document (make-text-document-identifier uri)
                   :options (make-instance 'lsp:formatting-options
                                           :tab-size tab-size
                                           :insert-spaces insert-spaces)))))

(defun call-document-highlight-request (uri &key (line 0) (character 0))
  "Call the document highlight request and return the response."
  (call-lsp-method
   (make-instance 'document-highlight-request)
   (convert-to-json
    (make-instance 'lsp:document-highlight-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)))))

(defun call-signature-help-request (uri &key (line 0) (character 0))
  "Call the signature help request and return the response."
  (call-lsp-method
   (make-instance 'signature-help-request)
   (convert-to-json
    (make-instance 'lsp:signature-help-params
                   :text-document (make-text-document-identifier uri)
                   :position (make-position line character)))))
