(in-package :lem-mcp-server)

;;; Lem Commands for MCP Server Control

(define-command mcp-server-start (port) ((:number "Port: "))
  "Start the MCP HTTP server on the specified port.
If no port is specified, uses *mcp-server-default-port* (default: 7890).

After starting, connect from Claude Code with:
  claude mcp add --transport http lem http://127.0.0.1:PORT/mcp"
  (let ((actual-port (or port *mcp-server-default-port*)))
    (when (current-mcp-server)
      (message "MCP server is already running on port ~A"
               (mcp-server-port (current-mcp-server)))
      (return-from mcp-server-start))
    (handler-case
        (let ((server (make-instance 'mcp-server :port actual-port)))
          (start-mcp-server server)
          (message "MCP server started at http://127.0.0.1:~A/mcp" actual-port))
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
          (message "MCP server running at http://127.0.0.1:~A/mcp (~A session~:P)"
                   (mcp-server-port server)
                   num-sessions))
        (message "MCP server is not running"))))

(define-command mcp-server-restart (port) ((:number "Port: "))
  "Restart the MCP server, optionally on a new port."
  (when (current-mcp-server)
    (let ((old-port (mcp-server-port (current-mcp-server))))
      (stop-mcp-server (current-mcp-server))
      (unless port
        (setf port old-port))))
  (let ((actual-port (or port *mcp-server-default-port*)))
    (handler-case
        (let ((server (make-instance 'mcp-server :port actual-port)))
          (start-mcp-server server)
          (message "MCP server restarted at http://127.0.0.1:~A/mcp" actual-port))
      (error (e)
        (message "Failed to restart MCP server: ~A" e)))))

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
