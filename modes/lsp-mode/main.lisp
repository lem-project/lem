(defpackage :lem-lsp-mode/main
  (:use :cl :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/json-lsp-utils
                :coerce-json)
  (:import-from :jsonrpc))
(in-package :lem-lsp-mode/main)

(cl-package-locks:lock-package :lem-lsp-mode/main)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)

(defclass initialize-request ()
  ((root-uri
    :initform nil
    :initarg :root-uri
    :reader initialize-request-root-uri)))

(defmethod lsp-call-method ((request initialize-request) client)
  (coerce-json (jsonrpc:call (client-connection client)
                             "initialize"
                             (object-to-json
                              (make-instance 'protocol:initialize-params
                                             :process-id (utils:get-pid)
                                             :client-info (make-json :name "lem" #|:version "0.0.0"|#)
                                             :root-uri (initialize-request-root-uri request)
                                             :capabilities (make-instance 'protocol:client-capabilities
                                                                          :workspace (make-json :apply-edit nil
                                                                                                :workspace-edit nil
                                                                                                :did-change-configuration nil
                                                                                                :symbol nil
                                                                                                :execute-command nil)
                                                                          :text-document nil
                                                                          :experimental nil)
                                             :trace nil
                                             :workspace-folders nil)))
               'protocol:initialize-result))


(defclass server-info ()
  ((name
    :initarg :name)
   (version
    :initarg :version)))

(defun json-to-server-info (json)
  (make-instance 'server-info
                 :name (json-get json "name")
                 :version (json-get json "version")))


(defgeneric jsonrpc-connect (client))

(defclass client ()
  ((connection
    :initform (jsonrpc:make-client)
    :reader client-connection)
   (server-info
    :initarg :server-info
    :type server-info
    :writer set-server-info)
   (server-capabilities
    :initarg :server-capabilities
    :type protocol:server-capabilities
    :writer set-server-capabilities)))

(defclass tcp-client (client)
  ((port
    :initarg :port
    :reader tcp-client-port)))

(defmethod jsonrpc-connect ((client tcp-client))
  (jsonrpc:client-connect (client-connection client)
                          :mode :tcp
                          :port (tcp-client-port client)))

(defun root-uri-pattern-p (file-name)
  ;; TODO
  (equal "asd" (pathname-type file-name)))

(defun find-root-uri (editing-pathname)
  (labels ((find-root-uri-pattern-file (directory)
             (dolist (pathname (uiop:directory-files directory))
               (when (root-uri-pattern-p (file-namestring pathname))
                 (return directory))))
           (recursive (directory)
             (cond ((find-root-uri-pattern-file directory))
                   ((uiop:pathname-equal directory (user-homedir-pathname))
                    nil)
                   (t (recursive (uiop:pathname-parent-directory-pathname directory))))))
    (recursive (uiop:pathname-directory-pathname editing-pathname))))

(defun project-root-pathname (editing-pathname)
  (or (find-root-uri editing-pathname)
      (uiop:pathname-directory-pathname editing-pathname)))

(defun initialize-lsp (client initialize-request)
  (let* ((initialize-result (lsp-call-method initialize-request client))
         (server-info (protocol:initialize-result-server-info initialize-result))
         (server-capabilities (protocol:initialize-result-capabilities initialize-result)))
    (set-server-info (json-to-server-info server-info) client)
    (set-server-capabilities server-capabilities client)))

(defun main ()
  (let ((editing-pathname (asdf:system-relative-pathname :lem-lsp-mode "main.lisp")))
    (let ((client (make-instance 'tcp-client :port 2089)))
      (jsonrpc-connect client)
      (initialize-lsp client
                      (make-instance 'initialize-request
                                     :root-uri (utils:pathname-to-uri
                                                (project-root-pathname
                                                 editing-pathname))))
      client)))
