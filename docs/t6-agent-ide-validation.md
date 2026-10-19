# T6 resource lifecycle and agent IDE validation

## Behavior

Process effects launch without waiting on the UI thread. An output worker drains
stdout, posts buffer writes through the editor event queue, and reaps the child.
A buffer supplied as an object, or an existing named buffer, is borrowed. A named
buffer created by the activation is owned and deleted on undo. Undo invalidates
queued output writes, terminates the child, and clears the process handle.

TCP effects capture a separate listener state for every activation. Undo closes
the listener and shuts down accepted connections; handlers blocked on socket IO
then unwind and close their own streams. The acceptor polls with a bounded timeout
and exits without an unconditional thread join on the UI thread. Handlers must
cooperate with socket shutdown; arbitrary computation is not forcibly cancelled.
The handle's `:socket-of` accessor supports port-zero listeners.

Function effects capture baselines on each activation, warn on overrides and
unwind nested definitions in LIFO order. LSP effects stop only sessions they
created. `lem-lsp-mode:start-lsp-session` retains its workspace first value and
adds a second value indicating ownership. Pending workspaces are registered
immediately for reuse, while pending `:then` callbacks wait for initialization.
Stopping a workspace cancels pending initialization and clears its spinner so
queued callbacks cannot revive the stopped session.

The inherited LSP buffer-hook cleanup remains tracked separately in
[issue #35](https://github.com/Yuri-Albuquerque/yeditor/issues/35), related to #13:
completed initialization adds four hooks which are still present after stop.

## Checks performed

New tests reproduced stale function baselines and incorrect nested restoration,
missing process output/owned buffers, a retained process handle, a lingering TCP
worker, duplicate pending LSP sessions and a queued initialization on a disposed
client. After the changes, the following passed locally with SBCL 2.5.2:

- All Yeditor suites via `scripts/run-yeditor-all-tests.lisp`.
- T4/T5/T6 effect suites together and T6 alone via its ASDF test operation.
- 100 dummy-component process cycles, including `ps` confirmation that children
  were reaped and owned buffers removed; existing output buffers survive rollback.
- 100 TCP reactivations with real byte round trips, plus shutdown while an accepted
  handler is blocked reading, repeated undo and assertions that workers exit.
- 100 owned/borrowed LSP session cycles using the local Python stdio fixture;
  completed LSP initialization, delayed callbacks and cancellation after stop.
- Existing function reversal tests (200 cycles), activation-time capture and
  nested override assertions.
- Agent smoke tests: MCP handshake, tool discovery, capability rejection, worker
  registration, task/observation delivery over HTTP and SSE, LSP start/stop and
  missing-server feedback, and transactional agent HUD mount/unmount.
- License/L0 boundary gate, workflow YAML parsing and `git diff --check`.

The observed MCP handshake-plus-task-delivery p95 was 4 ms in the initial agent
smoke run (50 samples, existing threshold 50 ms); this is a local measurement.

## Reproduction

From the repository root, after dependency installation and toolchain activation:

```sh
sbcl --script scripts/run-yeditor-effects-tests.lisp
sbcl --script scripts/run-yeditor-agent-smoke-tests.lisp
sbcl --script scripts/run-yeditor-all-tests.lisp
```

The agent smoke runner sets up its own timer manager, so it works independently
of other suites; CI runs it separately. If the cloud HTTP proxy rejects loopback
requests, run these local HTTP integration checks with proxy environment variables
removed for that process:

```sh
env -u HTTP_PROXY -u HTTPS_PROXY -u ALL_PROXY \
    -u http_proxy -u https_proxy -u all_proxy \
    sbcl --script scripts/run-yeditor-agent-smoke-tests.lisp
```

These checks use the fake frontend and real local processes/sockets; no external
LLM, pyright installation, microphone or graphical frontend is exercised. The
platform frontend builds are validated separately by GitHub CI.
