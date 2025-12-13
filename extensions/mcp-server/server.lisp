(in-package :lem-mcp-server)

;;; MCP HTTP Server Implementation
;;; Implements Streamable HTTP transport per MCP specification 2025-06-18

(defvar *current-mcp-server* nil
  "The currently running MCP server instance.")

(defun current-mcp-server ()
  "Return the current MCP server instance, or nil if not running."
  *current-mcp-server*)

;; Session management
(defclass mcp-session ()
  ((id :initarg :id
       :reader session-id
       :type string)
   (initialized-p :initform nil
                  :accessor session-initialized-p)
   (client-capabilities :initform nil
                        :accessor session-client-capabilities)
   (client-info :initform nil
                :accessor session-client-info)
   (created-at :initform (get-universal-time)
               :reader session-created-at)
   (last-activity :initform (get-universal-time)
                  :accessor session-last-activity)))

(defun generate-session-id ()
  "Generate a unique session ID."
  (format nil "~A-~A-~A"
          (get-universal-time)
          (random 1000000)
          (sb-posix:getpid)))

;; Server class
(defclass mcp-server ()
  ((port :initarg :port
         :reader mcp-server-port)
   (acceptor :initform nil
             :accessor mcp-server-acceptor)
   (sessions :initform (make-hash-table :test 'equal)
             :accessor mcp-server-sessions)
   (lock :initform (bt:make-lock "mcp-server-lock")
         :reader mcp-server-lock)
   (method-handlers :initform (make-hash-table :test 'equal)
                    :reader mcp-server-method-handlers)))

(defmethod initialize-instance :after ((server mcp-server) &key)
  "Initialize the MCP server and register method handlers."
  (register-all-methods server))

(defun register-all-methods (server)
  "Register all MCP method handlers."
  (let ((handlers (mcp-server-method-handlers server)))
    (dolist (class *mcp-method-classes*)
      (let ((instance (make-instance class)))
        (setf (gethash (mcp-method-name instance) handlers) instance)))))

;; Session management
(defun get-or-create-session (server session-id)
  "Get existing session or create new one."
  (bt:with-lock-held ((mcp-server-lock server))
    (or (gethash session-id (mcp-server-sessions server))
        (let ((session (make-instance 'mcp-session :id session-id)))
          (setf (gethash session-id (mcp-server-sessions server)) session)
          session))))

(defun get-session (server session-id)
  "Get session by ID, or nil if not found."
  (bt:with-lock-held ((mcp-server-lock server))
    (gethash session-id (mcp-server-sessions server))))

(defun remove-session (server session-id)
  "Remove a session."
  (bt:with-lock-held ((mcp-server-lock server))
    (remhash session-id (mcp-server-sessions server))))

;; HTTP Handler
(defclass mcp-acceptor (hunchentoot:easy-acceptor)
  ((mcp-server :initarg :mcp-server
               :reader acceptor-mcp-server)))

(defun handle-mcp-request ()
  "Handle incoming MCP HTTP request."
  (let* ((server (acceptor-mcp-server hunchentoot:*acceptor*))
         (request-method (hunchentoot:request-method*))
         (content-type (hunchentoot:content-type*)))
    (cond
      ((eq request-method :post)
       (handle-post-request server))
      ((eq request-method :get)
       (handle-get-request server))
      ((eq request-method :options)
       (handle-options-request))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
       ""))))

(defun handle-options-request ()
  "Handle CORS preflight request."
  (setf (hunchentoot:header-out :access-control-allow-origin) "*")
  (setf (hunchentoot:header-out :access-control-allow-methods) "GET, POST, OPTIONS")
  (setf (hunchentoot:header-out :access-control-allow-headers)
        "Content-Type, Accept, Mcp-Protocol-Version, Mcp-Session-Id")
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")

(defun handle-post-request (server)
  "Handle POST request (client -> server messages)."
  (setf (hunchentoot:header-out :access-control-allow-origin) "*")
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (protocol-version (hunchentoot:header-in* :mcp-protocol-version))
         (session-id (hunchentoot:header-in* :mcp-session-id)))
    (handler-case
        (let* ((message (yason:parse body :object-as :alist))
               (method (cdr (assoc "method" message :test #'string=)))
               (id (cdr (assoc "id" message :test #'string=)))
               (params (cdr (assoc "params" message :test #'string=))))
          (cond
            ;; Initialize request - create new session
            ((string= method "initialize")
             (let* ((new-session-id (generate-session-id))
                    (session (get-or-create-session server new-session-id)))
               (setf *current-session* session)
               (let ((result (handle-method server method params id)))
                 (setf (hunchentoot:header-out :mcp-session-id) new-session-id)
                 (setf (hunchentoot:content-type*) "application/json")
                 result)))
            ;; Other requests - require session
            (t
             (unless session-id
               (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
               (return-from handle-post-request
                 (encode-error-response id +invalid-request+ "Missing Mcp-Session-Id header")))
             (let ((session (get-session server session-id)))
               (unless session
                 (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
                 (return-from handle-post-request
                   (encode-error-response id +invalid-request+ "Session not found")))
               (setf (session-last-activity session) (get-universal-time))
               (setf *current-session* session)
               (let ((result (handle-method server method params id)))
                 (setf (hunchentoot:content-type*) "application/json")
                 result)))))
      (error (e)
        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
        (encode-error-response nil +parse-error+ (format nil "Parse error: ~A" e))))))

(defun handle-get-request (server)
  "Handle GET request (SSE stream for server -> client)."
  ;; For now, we don't support server-initiated messages
  ;; Return 405 to indicate SSE is not available at this endpoint
  (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
  "")

(defvar *current-session* nil
  "The session for the current request.")

(defvar *lifecycle-methods* '("initialize" "notifications/initialized" "shutdown" "exit" "ping"
                               "notifications/cancelled" "notifications/progress")
  "Methods that don't need to run in Lem's main thread.")

(defun lifecycle-method-p (method)
  "Return T if METHOD is a lifecycle method that doesn't need main thread."
  (member method *lifecycle-methods* :test #'string=))

(defun handle-method (server method params id)
  "Handle a JSON-RPC method call."
  (let ((handler (gethash method (mcp-server-method-handlers server))))
    (if handler
        (handler-case
            (let ((result (if (lifecycle-method-p method)
                              ;; Lifecycle methods run directly
                              (call-mcp-method handler params)
                              ;; Other methods run in Lem's main thread
                              (execute-in-editor
                               (lambda ()
                                 (call-mcp-method handler params))))))
              (if id
                  (encode-response id result)
                  ;; Notification - no response needed
                  (progn
                    (setf (hunchentoot:return-code*) hunchentoot:+http-accepted+)
                    "")))
          (mcp-error (e)
            (encode-error-response id (mcp-error-code e) (mcp-error-message e)))
          (error (e)
            (encode-error-response id +internal-error+ (format nil "~A" e))))
        (encode-error-response id +method-not-found+
                               (format nil "Method not found: ~A" method)))))

(defun alist-to-hash-table (alist)
  "Recursively convert an alist to a hash-table for JSON encoding."
  (cond
    ;; Boolean values
    ((eq alist t) 'yason:true)
    ((eq alist :false) 'yason:false)
    ((eq alist :true) 'yason:true)
    ;; nil -> null (let yason handle it)
    ((null alist) :null)
    ;; Alist with string keys -> hash-table
    ((and (consp alist) (consp (car alist)) (stringp (caar alist)))
     (let ((ht (make-hash-table :test 'equal)))
       (dolist (pair alist)
         (setf (gethash (car pair) ht)
               (alist-to-hash-table (cdr pair))))
       ht))
    ;; List -> array
    ((consp alist)
     (mapcar #'alist-to-hash-table alist))
    ;; Other values pass through
    (t alist)))

(defun encode-response (id result)
  "Encode a successful JSON-RPC response."
  (with-output-to-string (s)
    (yason:encode
     (alist-to-hash-table
      `(("jsonrpc" . "2.0")
        ("id" . ,id)
        ("result" . ,result)))
     s)))

(defun encode-error-response (id code message)
  "Encode a JSON-RPC error response."
  (with-output-to-string (s)
    (yason:encode
     (alist-to-hash-table
      `(("jsonrpc" . "2.0")
        ("id" . ,id)
        ("error" . (("code" . ,code)
                    ("message" . ,message)))))
     s)))

;; Server lifecycle
(defun start-mcp-server (server)
  "Start the MCP HTTP server."
  (when *current-mcp-server*
    (error "MCP server is already running"))
  (let ((port (mcp-server-port server)))
    (setf (mcp-server-acceptor server)
          (make-instance 'mcp-acceptor
                         :port port
                         :address "127.0.0.1"
                         :mcp-server server))
    ;; Set up the request dispatcher
    (setf hunchentoot:*dispatch-table*
          (list (hunchentoot:create-prefix-dispatcher "/mcp" 'handle-mcp-request)))
    (hunchentoot:start (mcp-server-acceptor server))
    (setf *current-mcp-server* server)
    server))

(defun stop-mcp-server (server)
  "Stop the MCP HTTP server."
  (when server
    (when (mcp-server-acceptor server)
      (ignore-errors
        (hunchentoot:stop (mcp-server-acceptor server)))
      (setf (mcp-server-acceptor server) nil))
    (when (eq *current-mcp-server* server)
      (setf *current-mcp-server* nil))))

;; Execute in Lem's main thread
(defun execute-in-editor (function)
  "Execute FUNCTION in Lem's main thread and wait for result."
  (let ((result-lock (bt:make-lock))
        (result-cv (bt:make-condition-variable))
        (result nil)
        (result-ready nil)
        (error-occurred nil)
        (error-value nil))
    (send-event
     (lambda ()
       (handler-case
           (setf result (funcall function))
         (error (e)
           (setf error-occurred t
                 error-value e)))
       (bt:with-lock-held (result-lock)
         (setf result-ready t)
         (bt:condition-notify result-cv))))
    (bt:with-lock-held (result-lock)
      (loop :until result-ready
            :do (bt:condition-wait result-cv result-lock)))
    (if error-occurred
        (error error-value)
        result)))
