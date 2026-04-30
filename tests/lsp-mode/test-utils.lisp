(defpackage :lem-tests/lsp-mode/test-utils
  (:use :cl
        :lem
        :lem-lsp-mode)
  (:import-from :lem-tests/lsp-mode/mock-client
                :mock-client
                :make-mock-client
                :set-mock-client-response
                :mock-client-request-history)
  (:import-from :lem-lsp-base/type
                :make-lsp-array)
  (:export :with-mock-lsp-workspace
           :make-test-server-capabilities
           :make-mock-completion-response
           :make-mock-hover-response
           :make-test-completion-params
           :make-test-hover-params
           :get-request-history))
(in-package :lem-tests/lsp-mode/test-utils)

(defun make-test-server-capabilities (&key
                                        (hover-provider t)
                                        (completion-provider t)
                                        (definition-provider t)
                                        (references-provider t)
                                        (document-symbol-provider t)
                                        (document-formatting-provider t))
  "Create server capabilities for testing."
  (let ((capabilities (make-instance 'lsp:server-capabilities)))
    (when hover-provider
      (setf (lsp:server-capabilities-hover-provider capabilities) t))
    (when completion-provider
      (setf (lsp:server-capabilities-completion-provider capabilities)
            (make-instance 'lsp:completion-options
                           :trigger-characters (make-lsp-array "." ":"))))
    (when definition-provider
      (setf (lsp:server-capabilities-definition-provider capabilities) t))
    (when references-provider
      (setf (lsp:server-capabilities-references-provider capabilities) t))
    (when document-symbol-provider
      (setf (lsp:server-capabilities-document-symbol-provider capabilities) t))
    (when document-formatting-provider
      (setf (lsp:server-capabilities-document-formatting-provider capabilities) t))
    capabilities))

(defun make-test-spec ()
  "Create a test language spec."
  ;; Use internal package access since spec class is not exported
  (make-instance 'lem-lsp-mode/spec::spec
                 :language-id "test"
                 :root-uri-patterns '()
                 :command '("test-lsp-server")
                 :connection-mode :stdio
                 :mode nil))

(defun make-test-workspace (&key (client (make-mock-client))
                                 (capabilities (make-test-server-capabilities)))
  "Create a test workspace with mock client."
  (make-instance 'lem-lsp-mode::workspace
                 :client client
                 :spec (make-test-spec)
                 :root-uri "file:///test"
                 :server-capabilities capabilities))

(defmacro with-mock-lsp-workspace ((&key responses capabilities) &body body)
  "Execute BODY with a mock LSP workspace set up.
RESPONSES is an alist of (method-name . response) pairs.
CAPABILITIES is a server-capabilities object or nil for defaults."
  (let ((client-var (gensym "CLIENT"))
        (workspace-var (gensym "WORKSPACE")))
    `(let* ((,client-var (make-mock-client))
            (,workspace-var (make-test-workspace
                             :client ,client-var
                             :capabilities (or ,capabilities
                                               (make-test-server-capabilities)))))
       ,@(when responses
           `((loop :for (method . response) :in ,responses
                   :do (set-mock-client-response ,client-var method response))))
       (let ((lem-lsp-mode::*workspace-list-per-language-id*
               (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash "test" ht) (list ,workspace-var))
                 ht)))
         ,@body))))

;;; Mock response constructors

(defun make-mock-completion-response (items)
  "Create a mock completion response.
ITEMS should be a list of plists with :label, :kind, :detail."
  (make-lsp-array
   (loop :for item :in items
         :collect (make-instance 'lsp:completion-item
                                 :label (getf item :label "")
                                 :kind (getf item :kind lsp:completion-item-kind-text)
                                 :detail (getf item :detail "")))))

(defun make-mock-hover-response (contents)
  "Create a mock hover response with CONTENTS string."
  (make-instance 'lsp:hover
                 :contents contents))

;;; Test params constructors

(defun make-test-text-document-identifier (&optional (uri "file:///test/example.lisp"))
  "Create a text document identifier for testing."
  (make-instance 'lsp:text-document-identifier :uri uri))

(defun make-test-position (&optional (line 0) (character 0))
  "Create a position for testing."
  (make-instance 'lsp:position :line line :character character))

(defun make-test-completion-params (&key (uri "file:///test/example.lisp")
                                          (line 0) (character 0))
  "Create completion params for testing."
  (make-instance 'lsp:completion-params
                 :text-document (make-test-text-document-identifier uri)
                 :position (make-test-position line character)))

(defun make-test-hover-params (&key (uri "file:///test/example.lisp")
                                     (line 0) (character 0))
  "Create hover params for testing."
  (make-instance 'lsp:hover-params
                 :text-document (make-test-text-document-identifier uri)
                 :position (make-test-position line character)))

;;; Test introspection

(defun get-request-history (client)
  "Get the request history from a mock client."
  (mock-client-request-history client))
