# MCP Server for Lem

MCP (Model Context Protocol) server implementation for Lem editor. This allows AI agents to interact with Lem through a standardized protocol.

## Features

- Buffer operations (create, read, update, delete)
- Text editing (insert, replace, delete regions)
- Command listing and execution
- Resource access (buffer contents)

## Running Tests

```bash
# Build test image
docker build -f docker/Dockerfile.test -t lem-tests .

# Run tests
docker run --rm lem-tests
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
