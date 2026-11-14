# cl-lsp

The Common Lisp language server that ships with Lem. It speaks LSP over stdio
by default, or over tcp, and serves Lisp code through a micros backend.

## Backend address

By default the server starts its own backend with `ros run -s micros`.

To connect to a backend that is already running instead, set
`CL_LSP_BACKEND_PORT`. Set `CL_LSP_BACKEND_HOST` as well when the backend
does not run on the same host; it falls back to the `:backend-hostname`
setting, which is `localhost`.

| Variable | Meaning |
| --- | --- |
| `CL_LSP_BACKEND_HOST` | Host name of the running backend. Defaults to `localhost`. |
| `CL_LSP_BACKEND_PORT` | Port number of the running backend. When set, the server connects to it instead of starting a backend. |

This is what lets an installation without Roswell use cl-lsp.
