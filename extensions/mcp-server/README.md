# MCP Server for Lem

MCP (Model Context Protocol) server implementation for Lem editor. This allows AI agents to interact with Lem through a standardized protocol.

## Features

- Buffer operations (create, read, update, delete)
- Text editing (insert, replace, delete regions)
- Command listing and execution
- Resource access (buffer contents)

## Docker Development Environment

### Running Tests

```bash
# Build test image
docker build -f docker/Dockerfile.mcp-test -t lem-tests .

# Run tests
docker run --rm lem-tests
```

### Development Server (micros connection)

```bash
# Build development image
docker build -f docker/Dockerfile.mcp-dev -t lem-mcp-dev .

# Start server
docker run --rm -p 4005:4005 --name lem-mcp-dev lem-mcp-dev

# Start with source mounting (for live reload)
docker run --rm -p 4005:4005 \
  -v $(pwd)/extensions/mcp-server:/app/extensions/mcp-server \
  --name lem-mcp-dev lem-mcp-dev
```

### Connecting from Lem

```
M-x slime-connect RET
Host: 127.0.0.1 RET
Port: 4005 RET
```

> Note: The Docker container binds micros to `0.0.0.0` to allow external connections.

### REPL Operations

```lisp
;; Switch to package
(in-package :lem-mcp-server)

;; List available tools
(list-all-tools)

;; Reload after source changes (when mounted)
(asdf:load-system :lem-mcp-server :force t)

;; Start MCP server (creates and starts server instance)
(mcp-server-start 8080)

;; Stop server
(mcp-server-stop)
```

## Available Tools

| Tool | Description |
|------|-------------|
| `buffer_list` | List all buffers |
| `buffer_get_content` | Get buffer content |
| `buffer_create` | Create a new buffer |
| `buffer_delete` | Delete a buffer |
| `buffer_info` | Get buffer information |
| `buffer_insert` | Insert text at position |
| `buffer_replace` | Replace text in region |
| `buffer_delete_region` | Delete text in region |
| `buffer_set_content` | Replace entire buffer content |
| `command_list` | List available commands |

## Connecting from Claude Code

### 1. Start Lem and MCP Server

```bash
# Start Lem
nix run .#lem
# Or your preferred method

# In Lem, start the MCP server:
# M-x mcp-server-start RET 7890 RET
```

### 2. Register with Claude Code

```bash
claude mcp add --transport http lem http://127.0.0.1:7890/mcp
```

### 3. Verify Connection

```bash
# List registered servers
claude mcp list

# Test the connection (server must be running)
curl -X POST http://127.0.0.1:7890/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test"}}}'
```

### 4. Use in Claude Code

Once connected, Claude Code can use the following tools:
- `buffer_list`, `buffer_create`, `buffer_get_content`, etc.
- `buffer_insert`, `buffer_replace`, `buffer_delete_region`
- `command_list`

## MCP Protocol

This server implements MCP 2024-11-05 specification:

- `initialize` / `notifications/initialized` - Lifecycle
- `tools/list` / `tools/call` - Tool operations
- `resources/list` / `resources/read` - Resource access
