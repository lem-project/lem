# T7: conditional components and context events

Declare the plan's context requirements with `:requires` on the existing component
API (the same predicate plist accepted by `define-coeffects`):

```lisp
(yeditor-context:define-component python-tools
  :requires '(:buffer-mode lem-python-mode:python-mode :window-width 80)
  :install (lambda (component context)
             ;; Return reversible effect handles; CONTEXT contains :buffer, etc.
             ...))
```

All predicates in a plist must match. Supported T7 predicates are `:buffer-mode`,
`:project-type`, `:file-type`, `:remote-p`, `:server-online` and minimum
`:window-width`. Existing work predicates, `:any`, project roots and project
markers remain supported. Mode/project symbols use identity; file types use
string equality. Server events maintain independent status for each server key.
`:requires` takes precedence over legacy `define-coeffects` declarations.

The context bridge publishes real file-open and buffer-selection events, completed
`change-buffer-mode` calls, buffer deletion, and relevant post-command changes.
Post-command checks avoid project filesystem probes when the buffer/file/mode has
not changed. Background buffer mode/close events preserve the selected context.
Callers also publish project, remote, server and resize events through
`emit-context-event`. Configuration reload emits `:config-change`; the watcher
polls off-thread and schedules reconciliation on the editor event queue. Stopping
the watcher invalidates already queued reloads.

Components carry explicit lifecycle status, so an empty effect list is not
mistaken for an inactive component. Repeated installation is idempotent. Each
transition binds its own pending/committed transaction lists. Activation callback
failure rolls back committed effects. Failed deactivation restores completed
undos and retains the previous lifecycle state. Individual effect apply/undo
operations must themselves be atomic on failure and compensatable; compensation
errors propagate and are not reported as successful rollback.

When removing an older conditional override, newer retained conditional layers
are suspended in reverse activation order and resumed with the same handles in
forward order. Their effects capture new baselines, so final removal restores the
original resource instead of resurrecting an obsolete layer. Reentrant context
updates schedule another pass. Feedback that does not settle within the configured
pass budget signals `context-convergence-error`; it does not silently abandon the
latest context. Ordinary transition failures are returned with component and
condition information, while other independent transitions can proceed.

## Regression evidence

Tests first reproduced missing declarations, zero-effect state, mode changes that
did not deactivate, multiple-server status loss, incomplete inverse/deactivation
rollback, ignored activation callbacks, missing config events, and stale layered
overrides. Coverage includes:

- 300 context changes across six components, checking every component after every
  event and confirming that an outer pending transaction remains untouched.
- 100 real Python → Java → Python → delete-buffer cycles through the installed
  bridge, with no synthetic mode events. Buffer and bridge-hook snapshots must
  match their initial values after uninstall.
- All six T7 predicates, multiple online servers, background buffer isolation,
  explicit resize/project events and empty-component lifecycle/agent status.
- Failure injection for activation, inverses, deactivation and independent reactive
  components, plus nested feedback, non-convergence and retained-handle identity.
- A real config polling worker: activation occurs only when the editor processes
  its event queue, on the same thread as the editor.

Run the existing complete Yeditor suite from the repository root:

```sh
sbcl --script scripts/run-yeditor-all-tests.lisp
```

The focused ASDF target is `yeditor-context-reactivity-tests`, and real-buffer
integration belongs to `yeditor-components-tests`. Both are included in CI.
The cloud environment's local HTTP integration tests require direct loopback
connections (remove HTTP proxy variables for the test process if necessary).

The previous CI failure was traced to `ps` missing from the Docker test image;
`procps` is now an explicit image dependency. The assertions are retained. No
lockfiles or licenses changed. Headless tests exercise the real editor core and
local agent MCP flows; graphical/platform builds are checked separately in CI.
