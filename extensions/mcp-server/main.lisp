(in-package :lem-mcp-server)

;;; MCP Server Main Entry Point and Initialization

;; Cleanup on exit
(defun cleanup-mcp-server ()
  "Clean up MCP server on Lem exit."
  (when (current-mcp-server)
    (stop-mcp-server (current-mcp-server))))

;; Register cleanup with Lem's exit-hook
;; (add-hook lem:*exit-hook* 'cleanup-mcp-server)

;;; Summary of available tools
;;;
;;; Buffer Operations:
;;;   - buffer_list: Get list of all buffers
;;;   - buffer_get_content: Get buffer content
;;;   - buffer_create: Create a new buffer
;;;   - buffer_delete: Delete a buffer
;;;   - buffer_info: Get detailed buffer information
;;;
;;; Editing Operations:
;;;   - buffer_insert: Insert text at position
;;;   - buffer_delete_region: Delete text in a region
;;;   - buffer_replace: Replace text in a region
;;;   - buffer_save: Save buffer to file
;;;   - buffer_set_content: Replace entire buffer content
;;;
;;; Command Operations:
;;;   - command_list: List available commands
;;;   - command_execute: Execute a command
;;;   - command_info: Get command information
;;;   - eval_expression: Evaluate Lisp expression
;;;
;;; Resources:
;;;   - buffer://{name}: Buffer content
;;;   - file://{path}: File content
;;;
;;; Lem Commands:
;;;   - M-x mcp-server-start: Start MCP server
;;;   - M-x mcp-server-stop: Stop MCP server
;;;   - M-x mcp-server-status: Show server status
;;;   - M-x mcp-server-restart: Restart server
;;;
;;; Usage with Claude Code:
;;;   1. Start server in Lem: M-x mcp-server-start RET 7890 RET
;;;   2. Add to Claude Code:
;;;      claude mcp add --transport http lem http://127.0.0.1:7890/mcp
