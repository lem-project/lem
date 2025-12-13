(in-package :lem-mcp-server)

;;; MCP Lifecycle Methods
;;; Handles initialize, initialized, shutdown, and exit

;; Initialize request
(define-mcp-request (initialize-request "initialize") (params)
  (let ((protocol-version (cdr (assoc "protocolVersion" params :test #'string=)))
        (client-capabilities (cdr (assoc "capabilities" params :test #'string=)))
        (client-info (cdr (assoc "clientInfo" params :test #'string=))))
    ;; Store client info in session
    (when *current-session*
      (setf (session-client-capabilities *current-session*) client-capabilities)
      (setf (session-client-info *current-session*) client-info))
    ;; Return server capabilities
    `(("protocolVersion" . ,*mcp-protocol-version*)
      ("capabilities" . ,(make-server-capabilities))
      ("serverInfo" . ,(make-server-info)))))

;; Initialized notification
(define-mcp-notification (initialized-notification "notifications/initialized") (params)
  (when *current-session*
    (setf (session-initialized-p *current-session*) t))
  nil)

;; Shutdown request
(define-mcp-request (shutdown-request "shutdown") (params)
  ;; Mark session as shutting down
  nil)

;; Exit notification
(define-mcp-notification (exit-notification "exit") (params)
  ;; Session will be cleaned up
  nil)

;; Ping request (for keepalive)
(define-mcp-request (ping-request "ping") (params)
  ;; Simple ping response
  nil)

;; Cancelled notification (for request cancellation)
(define-mcp-notification (cancelled-notification "notifications/cancelled") (params)
  ;; Handle cancellation request
  ;; For now, we don't support cancellation
  nil)

;; Progress notification (for long-running operations)
(define-mcp-notification (progress-notification "notifications/progress") (params)
  ;; Handle progress updates
  nil)
