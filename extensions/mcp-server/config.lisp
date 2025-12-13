(in-package :lem-mcp-server)

(defvar *mcp-server-default-port* 7890
  "Default port for MCP server.")

(defvar *mcp-server-default-hostname* "localhost"
  "Default hostname for MCP server.
Use \"0.0.0.0\" to listen on all interfaces.")

(defvar *mcp-protocol-version* "2024-11-05"
  "MCP protocol version supported by this server.")

(defvar *mcp-server-name* "lem-mcp-server"
  "Name of this MCP server.")

(defvar *mcp-server-version* "0.1.0"
  "Version of this MCP server.")
