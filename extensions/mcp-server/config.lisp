(in-package :lem-mcp-server)

(defvar *mcp-server-auto-start* nil
  "When non-nil, automatically start MCP server when Lem starts.")

(defvar *mcp-server-default-port* 7890
  "Default port for MCP server.")

(defvar *mcp-protocol-version* "2024-11-05"
  "MCP protocol version supported by this server.")

(defvar *mcp-server-name* "lem-mcp-server"
  "Name of this MCP server.")

(defvar *mcp-server-version* "0.1.0"
  "Version of this MCP server.")
