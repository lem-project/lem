(defpackage :lem-tests/language-server/test-utils
  (:use :cl
        :lem-language-server
        :lem-tests/language-server/utils)
  (:import-from :lem-lsp-base/type
                :make-lsp-map)
  (:import-from :lem-lsp-base/converter
                :convert-to-json
                :convert-from-json)
  (:export :with-mock-lsp-server
           :make-test-document
           :setup-completion-responses
           :setup-hover-response
           :setup-definitions-response
           :setup-references-response
           :setup-symbol-informations-response
           :make-mock-completion-item
           :make-mock-definition-location))
(in-package :lem-tests/language-server/test-utils)

;;; Test document setup

(defun call-initialize-request ()
  "Initialize the mock server with default client capabilities."
  (call-lsp-method
   (make-instance 'initialize-request)
   (convert-to-json
    (make-instance 'lsp:initialize-params
                   :process-id (random 10000)
                   :client-info (make-lsp-map :name "test-client"
                                              :version "1.0.0")
                   :root-uri "file:///test/"
                   :root-path "/test/"
                   :capabilities (lem-lsp-mode::client-capabilities)
                   :trace "off"
                   :workspace-folder :null))))

(defun call-did-open-request (&key uri language-id version text)
  "Open a text document in the mock server."
  (call-lsp-method
   (make-instance 'text-document-did-open-request)
   (convert-to-json
    (make-instance 'lsp:did-open-text-document-params
                   :text-document (make-instance 'lsp:text-document-item
                                                 :uri uri
                                                 :language-id language-id
                                                 :version version
                                                 :text text)))))

(defun make-test-document (&key (uri "file:///test/example.lisp")
                                (language-id "lisp")
                                (version 1)
                                (text "(defun test-fn (x) x)"))
  "Create a test document and return the text-document object."
  (call-did-open-request :uri uri
                         :language-id language-id
                         :version version
                         :text text)
  (find-text-document (make-instance 'lsp:text-document-identifier :uri uri)))

;;; Mock response setup

(defun setup-completion-responses (items)
  "Set up mock completion responses.
ITEMS should be a list of plists with :label, :kind, :signature, :documentation, :sort-text."
  (set-mock-response
   'micros/lsp-api:completions
   (mapcar (lambda (item)
             (list (getf item :label)
                   (getf item :kind :function)
                   (getf item :signature)
                   (getf item :documentation "")
                   (getf item :sort-text "0000")))
           items)))

(defun setup-hover-response (text)
  "Set up a mock hover response with TEXT."
  (set-mock-response 'micros/lsp-api:hover-symbol text))

(defun setup-definitions-response (definitions)
  "Set up mock definitions response.
DEFINITIONS should be a list of (dspec location) pairs where location is
a (:location (:file filename) (:position pos) hints) form."
  (set-mock-response 'micros:find-definitions-for-emacs definitions))

(defun setup-references-response (references)
  "Set up mock references response.
REFERENCES should be a list of (type . definitions) pairs."
  (set-mock-response 'micros:xrefs references))

(defun setup-symbol-informations-response (symbols)
  "Set up mock symbol informations response.
SYMBOLS should be a list of symbol-information objects."
  (set-mock-response 'micros/lsp-api:symbol-informations symbols))

;;; Mock data constructors

(defun make-mock-completion-item (&key label (kind :function) signature (documentation "") (sort-text "0000"))
  "Create a mock completion item plist."
  (list :label label
        :kind kind
        :signature signature
        :documentation documentation
        :sort-text sort-text))

(defun make-mock-definition-location (filename position &optional hints)
  "Create a mock definition location.
Returns a form suitable for definitions-at-point."
  `(:location (:file ,filename) (:position ,position) ,hints))

;;; Combined test setup macro

(defmacro with-mock-lsp-server ((&key completions hover definitions references
                                       symbol-informations)
                                &body body)
  "Set up a mock LSP server with optional canned responses.
Initializes the server, sets up any specified mock responses, and executes BODY."
  `(with-mock-server ()
     (call-initialize-request)
     ,@(when completions
         `((setup-completion-responses ,completions)))
     ,@(when hover
         `((setup-hover-response ,hover)))
     ,@(when definitions
         `((setup-definitions-response ,definitions)))
     ,@(when references
         `((setup-references-response ,references)))
     ,@(when symbol-informations
         `((setup-symbol-informations-response ,symbol-informations)))
     ,@body))
