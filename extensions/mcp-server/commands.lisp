(in-package :lem-mcp-server)

;;; Lem Commands for MCP Server Control

(define-command mcp-server-start (hostname port)
    ((:splice (list (lem:prompt-for-string "Hostname: "
                                            :initial-value *mcp-server-default-hostname*)))
     (:number "Port: "))
  "Start the MCP HTTP server on the specified hostname and port.
If no hostname is specified, uses *mcp-server-default-hostname* (default: 127.0.0.1).
If no port is specified, uses *mcp-server-default-port* (default: 7890).

After starting, connect from Claude Code with:
  claude mcp add --transport http lem http://HOSTNAME:PORT/mcp"
  (let ((actual-hostname (if (or (null hostname) (string= hostname ""))
                             *mcp-server-default-hostname*
                             hostname))
        (actual-port (or port *mcp-server-default-port*)))
    (when (current-mcp-server)
      (message "MCP server is already running on ~A:~A"
               (mcp-server-hostname (current-mcp-server))
               (mcp-server-port (current-mcp-server)))
      (return-from mcp-server-start))
    (handler-case
        (let ((server (make-instance 'mcp-server
                                     :hostname actual-hostname
                                     :port actual-port)))
          (start-mcp-server server)
          (message "MCP server started at http://~A:~A/mcp" actual-hostname actual-port))
      (error (e)
        (message "Failed to start MCP server: ~A" e)))))

(define-command mcp-server-stop () ()
  "Stop the running MCP server."
  (let ((server (current-mcp-server)))
    (if server
        (progn
          (stop-mcp-server server)
          (message "MCP server stopped"))
        (message "MCP server is not running"))))

(define-command mcp-server-status () ()
  "Show the current status of the MCP server."
  (let ((server (current-mcp-server)))
    (if server
        (let ((num-sessions (hash-table-count (mcp-server-sessions server))))
          (message "MCP server running at http://~A:~A/mcp (~A session~:P)"
                   (mcp-server-hostname server)
                   (mcp-server-port server)
                   num-sessions))
        (message "MCP server is not running"))))

(define-command mcp-server-restart (hostname port)
    ((:splice (list (lem:prompt-for-string "Hostname: "
                                            :initial-value *mcp-server-default-hostname*)))
     (:number "Port: "))
  "Restart the MCP server, optionally on a new hostname and port."
  (let ((old-hostname *mcp-server-default-hostname*)
        (old-port *mcp-server-default-port*))
    (when (current-mcp-server)
      (setf old-hostname (mcp-server-hostname (current-mcp-server)))
      (setf old-port (mcp-server-port (current-mcp-server)))
      (stop-mcp-server (current-mcp-server)))
    (let ((actual-hostname (if (or (null hostname) (string= hostname ""))
                               old-hostname
                               hostname))
          (actual-port (or port old-port)))
      (handler-case
          (let ((server (make-instance 'mcp-server
                                       :hostname actual-hostname
                                       :port actual-port)))
            (start-mcp-server server)
            (message "MCP server restarted at http://~A:~A/mcp" actual-hostname actual-port))
        (error (e)
          (message "Failed to restart MCP server: ~A" e))))))

(define-command mcp-server-toggle-logging () ()
  "Toggle MCP server logging to *mcp-log* buffer."
  (if *mcp-log-stream*
      (progn
        (setf *mcp-log-stream* nil)
        (message "MCP logging disabled"))
      (let ((buffer (make-buffer "*mcp-log*")))
        (setf *mcp-log-stream*
              (make-string-output-stream))
        (message "MCP logging enabled to *mcp-log* buffer"))))
